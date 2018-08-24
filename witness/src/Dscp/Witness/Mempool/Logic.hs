
module Dscp.Witness.Mempool.Logic
    ( MempoolVar
    , newMempoolVar
    , addTxToMempool
    , takeTxsMempool
    , isInMempool
    , normalizeMempool
    , readFromMempoolLocked

      -- * Helpers
    , onlyLostTxs
    ) where

import Control.Monad.Except (catchError)
import UnliftIO (MonadUnliftIO)

import qualified Data.Set as S
import Loot.Base.HasLens (HasLens', lensOf)

import qualified Snowdrop.Core as SD
import qualified Snowdrop.Execution as SD (dmaAccessActions)
import qualified Snowdrop.Execution as Pool
import qualified Snowdrop.Execution as AVLP
import qualified Snowdrop.Util as SD

import Dscp.Core.Foundation (GTxWitnessed)
import Dscp.Core.Foundation.Witness
import qualified Dscp.Snowdrop as SD
import Dscp.Snowdrop.Configuration (Exceptions, Ids, Values)
import Dscp.Witness.Mempool.Type
import Dscp.Witness.SDLock

type MempoolCtx ctx m =
    ( MonadIO         m
    , MonadUnliftIO   m
    , MonadReader ctx m
    , HasLens' ctx MempoolVar
    , HasLens' ctx SD.SDActions
    , HasLens' ctx SD.LoggingIO
    )

newMempoolVar :: MonadIO m => m MempoolVar
newMempoolVar = do
    pool <- Pool.createMempool
    let conf = Pool.defaultMempoolConfig SD.expandGTx SD.validator
    return (Mempool pool conf)

-- | Adds transaction to mempool. Make sure it's not there yet.
addTxToMempool
    :: forall ctx m
    .  (MempoolCtx ctx m, WithinWriteSDLock)
    => GTxWitnessed
    -> m Bool
addTxToMempool tx = do
    Mempool pool conf <- view (lensOf @MempoolVar)
    writeToMempool @ctx pool $ do
        Pool.processTxAndInsertToMempool conf tx
        return True
      `catchError` \_ -> do
        return False

-- | Take all mempool transactions, leaving it empty.
takeTxsMempool
    :: forall ctx m
    .  (MempoolCtx ctx m, WithinWriteSDLock)
    => m [GTxWitnessed]
takeTxsMempool = do
    Mempool pool _ <- view (lensOf @MempoolVar)
    txsWithUndos <- writeToMempool @ctx pool Pool.evictMempool
    return (map fst txsWithUndos)

-- | Check whether is this transaction already in mempool.
isInMempool
    :: forall ctx m
    .  (MempoolCtx ctx m, WithinReadSDLock)
    => GTxWitnessed -> m Bool
isInMempool tx = do
    Mempool pool _ <- view (lensOf @MempoolVar)
    txsWithUndos <- readFromMempool @ctx pool (gets Pool.msTxs)
    return (tx `elem` map fst txsWithUndos)

-- | Drop all transactions from mempool which are not valid with current
-- chain.
normalizeMempool
    :: forall ctx m
    .  (MempoolCtx ctx m, WithinWriteSDLock)
    => m (Pool.Rejected GTxWitnessed)
normalizeMempool = do
    Mempool pool conf <- view (lensOf @MempoolVar)
    writeToMempool pool $ Pool.normalizeMempool conf

type SDM =
    SD.ERwComp
        Exceptions
        Ids
        Values
        (SD.IOCtx ChgAccum)
        (Pool.MempoolState
            Ids
            Values
            ChgAccum
            GTxWitnessed)

readFromMempool
    :: forall ctx m a
    .  (MempoolCtx ctx m, WithinReadSDLock)
    => Pool.Mempool Ids Values ChgAccum GTxWitnessed
    -> SDM a
    -> m a
readFromMempool pool action = do
    actions <- view (lensOf @SD.SDActions)
    logger  <- view (lensOf @SD.LoggingIO)
    let dbActions = SD.dmaAccessActions $ SD.nsStateDBActions actions (AVLP.RememberForProof False)
    SD.runRIO logger $
        Pool.actionWithMempool pool dbActions action

readFromMempoolLocked
    :: forall ctx m a
    .  MempoolCtx ctx m
    => Pool.Mempool Ids Values ChgAccum GTxWitnessed
    -> SDLock
    -> SDM a
    -> m a
readFromMempoolLocked pool lock action = do
    readingSDLockOf lock $
        readFromMempool pool action

writeToMempool
    :: forall ctx m a
    .  (MempoolCtx ctx m, WithinWriteSDLock)
    => Pool.Mempool Ids Values ChgAccum GTxWitnessed
    -> SDM a
    -> m a
writeToMempool pool action = do
    actions <- view (lensOf @SD.SDActions)
    logger  <- view (lensOf @SD.LoggingIO)
    let dbActions = SD.dmaAccessActions $ SD.nsStateDBActions actions (AVLP.RememberForProof False)
    SD.runRIO logger $
        Pool.actionWithMempool pool dbActions action

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | After normalization, take those transactions which neither occur in new
-- blocks nor remain valid with respect to current chain.
onlyLostTxs :: Pool.Rejected GTxWitnessed -> [BlockBody] -> [GTxId]
onlyLostTxs (Pool.Rejected dropped) newBlocksBodies =
    let applied = fold $ map bbTxs newBlocksBodies
        lost = toList $ S.fromList dropped `S.difference` S.fromList applied
    in map (toGTxId . unGTxWitnessed) lost


module Dscp.Witness.Mempool.Logic
    ( MempoolVar
    , MempoolCtx
    , newMempoolVar
    , addTxToMempool
    , takeTxsMempool
    , readTxsMempool
    , normalizeMempool
    , runSdMempool
    , runSdMempoolLocked

      -- * Helpers
    , onlyLostTxs
    ) where

import UnliftIO (MonadUnliftIO)

import qualified Data.Set as S
import Loot.Base.HasLens (HasCtx, HasLens', lensOf)
import Loot.Log (MonadLogging)

import qualified Snowdrop.Core as SD
import qualified Snowdrop.Execution as Pool
import qualified Snowdrop.Util as SD

import Dscp.Core.Config
import Dscp.Core.Foundation (GTxWitnessed)
import Dscp.Core.Foundation.Witness
import Dscp.Crypto
import qualified Dscp.Snowdrop as SD
import Dscp.Snowdrop.Actions (sdActionsComposition)
import Dscp.Snowdrop.Configuration (Exceptions, Ids, Values)
import Dscp.Snowdrop.Mode
import Dscp.Witness.Mempool.Type
import Dscp.Witness.SDLock

type MempoolCtx ctx m =
    ( MonadIO         m
    , MonadUnliftIO   m
    , MonadReader ctx m
    , MonadLogging    m
    , HasLens' ctx MempoolVar
    , HasLens' ctx SD.SDVars
    , HasLens' ctx SD.LoggingIO
    , HasCoreConfig
    )

newMempoolVar :: (HasCoreConfig, MonadIO m) => PublicKey -> m MempoolVar
newMempoolVar pk = do
    pool     <- Pool.createMempool
    let conf = Pool.defaultMempoolConfig (SD.expandGTx pk) SD.validator
    return (Mempool pool conf)

-- | Adds transaction to mempool. Makes sure it's not there yet.
addTxToMempool
    :: forall ctx m
    .  (MempoolCtx ctx m, WithinWriteSDLock)
    => GTxWitnessed
    -> m ()
addTxToMempool tx = do
    Mempool pool conf <- view (lensOf @MempoolVar)
    writeToMempool @ctx pool $
        Pool.processTxAndInsertToMempool conf tx

-- | See all mempool transactions.
readTxsMempool :: (MempoolCtx ctx m, WithinReadSDLock) => m [GTxWitnessed]
readTxsMempool = do
    Mempool pool _ <- view (lensOf @MempoolVar)
    readFromMempoolUnsafe pool $
        map fst <$> gets Pool.msTxs

-- | Take all mempool transactions, leaving it empty.
takeTxsMempool
    :: forall ctx m
    .  (MempoolCtx ctx m, WithinWriteSDLock)
    => m [GTxWitnessed]
takeTxsMempool = do
    Mempool pool _ <- view (lensOf @MempoolVar)
    txsWithUndos <- writeToMempool @ctx pool Pool.evictMempool
    return (map fst txsWithUndos)

-- | Drop all transactions from mempool which are not valid with current
-- chain.
normalizeMempool
    :: forall ctx m
    .  (MempoolCtx ctx m, WithinWriteSDLock)
    => m (Pool.Rejected GTxWitnessed)
normalizeMempool = do
    Mempool pool conf <- view (lensOf @MempoolVar)
    writeToMempool pool $ Pool.normalizeMempool conf

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

-- | Read-only action with mempool.
type SdMemReadM = SdM_ ChgAccum

-- | Read-write action with mempool.
type SdMemWriteM =
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

-- | We expect that some actions can be run with both 'runSdM' and 'runSdMempoolLocked'
_sdMGeneralizationCheck :: ()
_sdMGeneralizationCheck =
    let _ = action :: SdMemReadM ()
        _ = SD.liftERoComp action :: SdMemWriteM ()
        _ = action :: SdM ()
    in ()
  where
    action :: SdM_ chgacc ()
    action = pass

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

-- | Perform an action with mempool in context.
-- Unsafe, because allows read-write actions but requires just a read lock.
readFromMempoolUnsafe
    :: forall ctx m a
    .  (MempoolCtx ctx m, WithinReadSDLock)
    => Pool.Mempool Ids Values ChgAccum GTxWitnessed
    -> SdMemWriteM a
    -> m a
readFromMempoolUnsafe pool action = do
    actions <- view (lensOf @SD.SDVars)
    logger  <- view (lensOf @SD.LoggingIO)
    let dbActions = sdActionsComposition actions
    SD.runRIO logger $
        SD.unwrapSDBaseRethrow $
        Pool.actionWithMempool pool dbActions action

-- | Perform a read-only action so that it takes the mempool into account.
readFromMempool
    :: forall ctx m a
    .  (MempoolCtx ctx m, WithinReadSDLock)
    => Pool.Mempool Ids Values ChgAccum GTxWitnessed
    -> SdMemReadM a
    -> m a
readFromMempool pool = readFromMempoolUnsafe pool . SD.liftERoComp

-- | Perform a read-write action, allowing modifying the mempool.
writeToMempool
    :: forall ctx m a
    .  (MempoolCtx ctx m, WithinWriteSDLock)
    => Pool.Mempool Ids Values ChgAccum GTxWitnessed
    -> SdMemWriteM a
    -> m a
writeToMempool = readFromMempoolUnsafe

-- | Similar to 'runSdM', but with the mempool taken into consideration.
runSdMempool
    :: (MempoolCtx ctx m, WithinReadSDLock)
    => SdMemReadM a -> m a
runSdMempool action = do
    Mempool pool _ <- view (lensOf @MempoolVar)
    readFromMempool pool action

runSdMempoolLocked
    :: (MempoolCtx ctx m, HasCtx ctx m '[SDLock])
    => SdMemReadM a -> m a
runSdMempoolLocked action = readingSDLock $ runSdMempool action

instance KnownSdReadMode 'ChainAndMempool where
    type SdReadModeChgAcc 'ChainAndMempool = ChgAccum
    liftSdM = lift . runSdMempool

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

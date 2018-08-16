
module Dscp.Witness.Mempool.Logic
    ( MempoolVar
    , newMempoolVar
    , addTxToMempool
    , takeTxsMempool
    , isInMempool
    ) where

import Dscp.Core.Foundation (GTxWitnessed)
import Dscp.Snowdrop.Configuration (Exceptions, Ids, Values)
import Dscp.Witness.Mempool.Type
import Loot.Base.HasLens (HasLens', lensOf)

import UnliftIO (MonadUnliftIO)

import qualified Dscp.Snowdrop as SD
import qualified Dscp.Snowdrop.Storage.Avlp as AVLP
import qualified Dscp.Witness.SDLock as Lock
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Model.Execution as SD (dmaAccessActions)
import qualified Snowdrop.Model.Mempool as Pool
import qualified Snowdrop.Util as SD

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
    lock <- Lock.newSDLock
    pool <- Pool.createMempool
    let conf = Pool.defaultMempoolConfig SD.expandGTx SD.validator
    return (Mempool pool conf, lock)

addTxToMempool
    :: forall ctx m
    .  MempoolCtx ctx m
    => MempoolVar
    -> GTxWitnessed
    -> m ()
addTxToMempool (Mempool pool conf, lock) tx =
    writeToMempool @ctx pool lock
        $ Pool.processTxAndInsertToMempool conf tx

takeTxsMempool
    :: forall ctx m
    .  MempoolCtx ctx m
    => MempoolVar
    -> m [GTxWitnessed]
takeTxsMempool (Mempool pool _, lock) = do
    txsWithUndos <- writeToMempool @ctx pool lock Pool.evictMempool
    return (map fst txsWithUndos)

isInMempool
    :: forall ctx m
    .  MempoolCtx ctx m
    => MempoolVar
    -> GTxWitnessed -> m Bool
isInMempool (Mempool pool _, lock) tx = do
    txsWithUndos <- readFromMempool @ctx pool lock (gets Pool.msTxs)
    return (tx `elem` map fst txsWithUndos)

type SDM =
    SD.ERwComp
        Exceptions
        Ids
        Values
        (SD.IOCtx (AVLP.AVLChgAccum Ids Values))
        (Pool.MempoolState
            Ids
            Values
            (AVLP.AVLChgAccum Ids Values)
            GTxWitnessed)

readFromMempool
    :: forall ctx m a
    .  MempoolCtx ctx m
    => Pool.Mempool Ids Values (AVLP.AVLChgAccum Ids Values) GTxWitnessed
    -> Lock.SDLock
    -> SDM a
    -> m a
readFromMempool pool lock action = do
    actions <- view (lensOf @SD.SDActions)
    logger  <- view (lensOf @SD.LoggingIO)
    let dbActions = SD.dmaAccessActions $ SD.nsStateDBActions actions (AVLP.RememberForProof False)
    Lock.readingSDLockOf lock $
        SD.runRIO logger $
            Pool.actionWithMempool pool dbActions action

writeToMempool
    :: forall ctx m a
    .  MempoolCtx ctx m
    => Pool.Mempool Ids Values (AVLP.AVLChgAccum Ids Values) GTxWitnessed
    -> Lock.SDLock
    -> SDM a
    -> m a
writeToMempool pool lock action = do
    actions <- view (lensOf @SD.SDActions)
    logger  <- view (lensOf @SD.LoggingIO)
    let dbActions = SD.dmaAccessActions $ SD.nsStateDBActions actions (AVLP.RememberForProof False)
    Lock.writingSDLockOf lock $
        SD.runRIO logger $
            Pool.actionWithMempool pool dbActions action

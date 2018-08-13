-- | Memory pool. TBD.

module Dscp.Witness.Mempool.Logic
    ( Mempool(..)
    , MempoolVar
    , newMempoolVar
    , addTxToMempool
    , takeTxsMempool
    , isInMempool
    ) where

import Dscp.Core.Foundation (GTxWitnessed)
import qualified Dscp.Snowdrop as SD
import qualified Dscp.Snowdrop.Configuration as Conf
import qualified Dscp.Snowdrop.Storage.Avlp as AVLP
import Dscp.Witness.Mempool.Type
import qualified Dscp.Witness.SDLock as Lock
import Loot.Base.HasLens (HasLens', lensOf)
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Model.Execution as SD (dmaAccessActions)
import qualified Snowdrop.Model.Mempool as Pool
import qualified Snowdrop.Util as SD
import qualified UnliftIO

newMempoolVar :: MonadIO m => m MempoolVar
newMempoolVar = do
    lock <- Lock.newSDLock
    pool <- Pool.createMempool
    let conf = Pool.defaultMempoolConfig SD.expandGTx SD.validator
    return (Mempool pool conf, lock)

addTxToMempool
    :: forall ctx m
    .  ( MonadIO m
       , MonadReader ctx m
       , HasLens' ctx MempoolVar
       , HasLens' ctx SD.SDActions
       , HasLens' ctx SD.LoggingIO
       , UnliftIO.MonadUnliftIO    m
       )
    => MempoolVar
    -> GTxWitnessed
    -> m ()
addTxToMempool (Mempool pool conf, lock) tx = do
    writeToMempool @ctx pool lock
        $ Pool.processTxAndInsertToMempool conf tx

takeTxsMempool
    :: forall ctx m
    .  ( MonadIO m
       , MonadReader ctx m
       , HasLens' ctx MempoolVar
       , HasLens' ctx SD.SDActions
       , HasLens' ctx SD.LoggingIO
       , UnliftIO.MonadUnliftIO    m
       )
    => MempoolVar
    -> m [GTxWitnessed]
takeTxsMempool (Mempool pool _, lock) = do
    txsWithUndos <- writeToMempool @ctx pool lock Pool.evictMempool
    return (map fst txsWithUndos)

isInMempool
    :: forall ctx m
    .  ( MonadIO m
       , MonadReader ctx m
       , HasLens' ctx MempoolVar
       , HasLens' ctx SD.SDActions
       , HasLens' ctx SD.LoggingIO
       , UnliftIO.MonadUnliftIO    m
       )
    => MempoolVar
    -> GTxWitnessed -> m Bool
isInMempool (Mempool pool _, lock) tx = do
    txsWithUndos <- readFromMempool @ctx pool lock (gets Pool.msTxs)
    return (tx `elem` map fst txsWithUndos)

type SDM =
    SD.ERwComp
        Conf.Exceptions
        Conf.Ids
        Conf.Values
        (SD.IOCtx (AVLP.AVLChgAccum Conf.Ids Conf.Values))
        (Pool.MempoolState
            Conf.Ids
            Conf.Values
            (AVLP.AVLChgAccum Conf.Ids Conf.Values)
            GTxWitnessed)

readFromMempool
    :: forall ctx m a
    .  ( MonadIO m
       , MonadReader ctx m
       , HasLens' ctx MempoolVar
       , HasLens' ctx SD.SDActions
       , HasLens' ctx SD.LoggingIO
       , UnliftIO.MonadUnliftIO    m
       )
    => Pool.Mempool Conf.Ids Conf.Values (AVLP.AVLChgAccum Conf.Ids Conf.Values) GTxWitnessed
    -> Lock.SDLock
    -> SDM a
    -> m a
readFromMempool pool lock action = do
    actions <- view (lensOf @SD.SDActions)
    logger  <- view (lensOf @SD.LoggingIO)
    let dbActions = SD.dmaAccessActions $ SD.nsStateDBActions actions (AVLP.RememberForProof False)
    Lock.readingSDLockOf lock $ do
        SD.runRIO logger $ Pool.actionWithMempool pool dbActions action

writeToMempool
    :: forall ctx m a
    .  ( MonadIO m
       , MonadReader ctx m
       , HasLens' ctx MempoolVar
       , HasLens' ctx SD.SDActions
       , HasLens' ctx SD.LoggingIO
       , UnliftIO.MonadUnliftIO    m
       )
    => Pool.Mempool Conf.Ids Conf.Values (AVLP.AVLChgAccum Conf.Ids Conf.Values) GTxWitnessed
    -> Lock.SDLock
    -> SDM a
    -> m a
writeToMempool pool lock action = do
    actions <- view (lensOf @SD.SDActions)
    logger  <- view (lensOf @SD.LoggingIO)
    let dbActions = SD.dmaAccessActions $ SD.nsStateDBActions actions (AVLP.RememberForProof False)
    Lock.writingSDLockOf lock $ do
        SD.runRIO logger $ Pool.actionWithMempool pool dbActions action

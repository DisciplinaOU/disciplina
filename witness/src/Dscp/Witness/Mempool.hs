-- | Memory pool. TBD.

module Dscp.Witness.Mempool
    ( Mempool(..)
    , MempoolVar
    , newMempoolVar
    , addTxToMempool
    , takeTxsMempool
    , isInMempool
    ) where

import Control.Concurrent.STM.TVar (modifyTVar, readTVar, swapTVar)
import Control.Lens (makeLenses)
import Dscp.Core.Foundation (GTxWitnessed)

data Mempool = Mempool
    { _mpTxs :: [GTxWitnessed]
    } deriving (Eq,Show)

makeLenses ''Mempool

type MempoolVar = TVar Mempool

newMempoolVar :: MonadIO m => m (TVar Mempool)
newMempoolVar = liftIO $ newTVarIO (Mempool mempty)

addTxToMempool :: MonadIO m => MempoolVar -> GTxWitnessed -> m ()
addTxToMempool var tx = atomically $ modifyTVar var $ mpTxs %~ (:) tx

takeTxsMempool :: MonadIO m => MempoolVar -> m [GTxWitnessed]
takeTxsMempool var = fmap (view mpTxs) $ atomically $ swapTVar var $ Mempool []

isInMempool :: MonadIO m => MempoolVar -> GTxWitnessed -> m Bool
isInMempool var tx = elem tx . _mpTxs <$> atomically (readTVar var)

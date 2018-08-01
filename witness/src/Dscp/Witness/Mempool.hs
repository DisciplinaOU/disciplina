-- | Memory pool. TBD.

module Dscp.Witness.Mempool
    ( Mempool(..)
    , MempoolVar
    , newMempoolVar
    , addTxToMempool
    , takeTxsMempool
    ) where

import Control.Concurrent.STM.TVar (modifyTVar, swapTVar)
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
addTxToMempool var tx = atomically $ modifyTVar var $ mpTxs %~ ((:) tx)

takeTxsMempool :: MonadIO m => MempoolVar -> m [GTxWitnessed]
takeTxsMempool var = fmap (view mpTxs) $ atomically $ swapTVar var $ Mempool []

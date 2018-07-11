-- | Memory pool. TBD.

module Dscp.Witness.Mempool
    ( Mempool(..)
    , MempoolVar
    , newMempoolVar
    ) where

import Dscp.Core (GlobalTx)

data Mempool = Mempool
    { mpTxs :: [GlobalTx]
    } deriving (Eq,Show)

type MempoolVar = TVar Mempool

newMempoolVar :: MonadIO m => m (TVar Mempool)
newMempoolVar = liftIO $ newTVarIO (Mempool mempty)

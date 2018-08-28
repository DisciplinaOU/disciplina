-- | Memory pool.

module Dscp.Witness.Mempool.Type
    ( Mempool(..)
    , MempoolVar
    , mpSDPool
    , mPSDConfig
    , ChgAccum
    ) where

import Control.Lens (makeLenses)
import qualified Snowdrop.Execution as Pool

import Dscp.Core.Foundation (GTxWitnessed)
import Dscp.Snowdrop.Configuration
import qualified Dscp.Snowdrop.IOCtx as SD
import Dscp.Snowdrop.Storage.Avlp as Avlp
import Dscp.Witness.AVL (AvlHash)

type ChgAccum =
    Pool.CompositeChgAccum
        (Pool.SumChangeSet Ids Values)
        (Avlp.AVLChgAccum AvlHash Ids Values)
        BlockPlusAVLComposition

type MempoolVar = Mempool (SD.IOCtx ChgAccum)

data Mempool ctx = Mempool
    { _mpSDPool   :: Pool.Mempool Ids Values ChgAccum GTxWitnessed
    , _mPSDConfig :: Pool.MempoolConfig Exceptions Ids Proofs Values ctx GTxWitnessed
    }

makeLenses ''Mempool

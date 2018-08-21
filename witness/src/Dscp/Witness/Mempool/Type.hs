
module Dscp.Witness.Mempool.Type
    ( Mempool(..)
    , MempoolVar
    , mpSDPool
    , mPSDConfig
    ) where

import Control.Lens (makeLenses)

import Dscp.Core.Foundation (GTxWitnessed)
import Dscp.Snowdrop.Configuration

import qualified Dscp.Snowdrop.IOCtx as SD
import qualified Dscp.Snowdrop.Storage.Avlp as AVLP
import qualified Snowdrop.Model.Mempool as Pool

type MempoolVar = Mempool (SD.IOCtx (AVLP.AVLChgAccum Ids Values))

data Mempool ctx = Mempool
    { _mpSDPool   :: Pool.Mempool Ids Values (AVLP.AVLChgAccum Ids Values) GTxWitnessed
    , _mPSDConfig :: Pool.MempoolConfig Exceptions Ids Proofs Values ctx GTxWitnessed
    }

makeLenses ''Mempool

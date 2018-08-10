-- | Memory pool. TBD.

module Dscp.Witness.Mempool.Type
    ( Mempool(..)
    , MempoolVar
    , mpSDPool
    , mPSDConfig
    ) where

import Control.Lens (makeLenses)
import Dscp.Core.Foundation (GTxWitnessed)
import qualified Dscp.Snowdrop.Configuration as Conf
import qualified Dscp.Snowdrop.IOCtx as SD
import qualified Dscp.Snowdrop.Storage.Avlp as AVLP
import qualified Dscp.Witness.SDLock as Lock
import qualified Snowdrop.Model.Mempool as Pool

type MempoolVar = MakeMempoolVar (SD.IOCtx (AVLP.AVLChgAccum Conf.Ids Conf.Values))

type MakeMempoolVar ctx = (Mempool ctx, Lock.SDLock)

data Mempool ctx = Mempool
    { _mpSDPool   :: Pool.Mempool Conf.Ids Conf.Values (AVLP.AVLChgAccum Conf.Ids Conf.Values) GTxWitnessed
    , _mPSDConfig :: Pool.MempoolConfig Conf.Exceptions Conf.Ids Conf.Proofs Conf.Values ctx GTxWitnessed
    }

makeLenses ''Mempool

{-# LANGUAGE StrictData #-}

module Dscp.Witness.Launcher.Params
       ( WitnessKeyParams (..)
       , WitnessParams (..)
       , wpLoggingParamsL
       , wpDBParamsL
       , wpNetworkParamsL
       , wpKeyParamsL
       , wpWebParamsL
       , wpAppDirParamL
       , wpMetricsEndpointL
       , WitnessWebParams (..)
       , wwpServerParamsL
       ) where

import Control.Lens (makeLensesWith)

import Dscp.DB.Rocks.Real.Types (RocksDBParams)
import Dscp.Resource.AppDir (AppDirParam)
import Dscp.Resource.Keys (BaseKeyParams, CommitteeParams)
import Dscp.Resource.Logging (LoggingParams)
import Dscp.Resource.Network (NetServParams)
import Dscp.Util
import Dscp.Web

-- | Witness key parameters.
data WitnessKeyParams = WitnessKeyParams
    { wkpBase      :: BaseKeyParams
    , wkpCommittee :: (Maybe CommitteeParams)
      -- ^ Optional committee params which may alter key generation.
    } deriving (Show)

-- | Witness API server parameters.
newtype WitnessWebParams = WitnessWebParams
    { wwpServerParams :: ServerParams
    } deriving (Show)

makeLensesWith postfixLFields ''WitnessWebParams

-- | Contains all initialization parameters of Witness node.
data WitnessParams = WitnessParams
    { wpLoggingParams   :: LoggingParams
    -- ^ Logging parameters.
    , wpDBParams        :: RocksDBParams
    -- ^ DB parameters
    , wpNetworkParams   :: NetServParams
    -- ^ Networking params.
    , wpKeyParams       :: WitnessKeyParams
    -- ^ Witness key params.
    , wpWebParams       :: Maybe WitnessWebParams
    -- ^ Witness server params, if need to serve it.
    , wpAppDirParam     :: AppDirParam
    -- ^ Application folder param
    , wpMetricsEndpoint :: MetricsEndpoint
    -- ^ Metrics endpoint
    } deriving (Show)

makeLensesWith postfixLFields ''WitnessParams

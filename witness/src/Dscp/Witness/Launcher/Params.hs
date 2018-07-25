{-# LANGUAGE StrictData #-}

module Dscp.Witness.Launcher.Params
       ( WitnessParams (..)
       ) where

import Dscp.DB.Rocks.Real.Types (RocksDBParams)
import Dscp.Resource.Keys (KeyParams)
import Dscp.Resource.Logging (LoggingParams)
import Dscp.Resource.Network (NetServParams)

-- | Contains all initialization parameters of Witness node.
data WitnessParams = WitnessParams
    { wpLoggingParams :: LoggingParams
    -- ^ Basic parameters for any node
    , wpDBParams      :: RocksDBParams
    -- ^ DB parameters
    , wpNetworkParams :: NetServParams
    -- ^ Networking params.
    , wpKeyParams     :: KeyParams
    -- ^ Key management params.
    } deriving Show

{-# LANGUAGE StrictData #-}

module Dscp.Witness.Launcher.Params
       ( WitnessKeyParams (..)
       , WitnessParams (..)
       ) where

import Dscp.DB.Rocks.Real.Types (RocksDBParams)
import Dscp.Resource.Keys (BaseKeyParams, CommitteeParams)
import Dscp.Resource.Logging (LoggingParams)
import Dscp.Resource.Network (NetServParams)
import Dscp.Web (ServerParams)

-- | Witness key parameters.
data WitnessKeyParams = WitnessKeyParams
    { wkpBase      :: BaseKeyParams
    , wkpCommittee :: (Maybe CommitteeParams)
      -- ^ Optional committee params which may alter key generation.
    } deriving (Show)

-- | Contains all initialization parameters of Witness node.
data WitnessParams = WitnessParams
    { wpLoggingParams       :: LoggingParams
    -- ^ Logging parameters.
    , wpDBParams            :: RocksDBParams
    -- ^ DB parameters
    , wpNetworkParams       :: NetServParams
    -- ^ Networking params.
    , wpKeyParams           :: WitnessKeyParams
    -- ^ Witness key params.
    , wpWitnessServerParams :: Maybe ServerParams
    -- ^ Witness server params, if need to serve it.
    } deriving Show

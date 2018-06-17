
-- | Resources used by Witness node

module Dscp.Witness.Launcher.Resource
       ( WitnessResources (..)
       ) where

import Universum

import System.Wlog (LoggerName)

import Dscp.DB.Real (NodeDB)
import Dscp.Launcher.Resource (AllocResource (..))
import Dscp.Witness.Launcher.Params (WitnessParams (..))

-- | Datatype which contains resources required by witness node to start
-- working.
data WitnessResources = WitnessResources
    { wrLoggerName :: !LoggerName
    , wrDB         :: !NodeDB
    }

instance AllocResource WitnessParams WitnessResources where
    allocResource WitnessParams{..} = do
        wrLoggerName <- allocResource wpLoggingParams
        wrDB <- allocResource wpDBParams
        return WitnessResources {..}


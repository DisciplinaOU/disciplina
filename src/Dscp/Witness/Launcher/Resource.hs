
-- | Resources used by Witness node

module Dscp.Witness.Launcher.Resource
       ( WitnessResources (..)
       ) where

import Universum

import Loot.Log.Rio (LoggingIO)

import Dscp.DB.Rocks.Real (NodeDB)
import Dscp.Launcher.Resource (AllocResource (..))
import Dscp.Witness.Launcher.Params (WitnessParams (..))

-- | Datatype which contains resources required by witness node to start
-- working.
data WitnessResources = WitnessResources
    { wrLogging :: !LoggingIO
    , wrDB      :: !NodeDB
    }

instance AllocResource WitnessParams WitnessResources where
    allocResource WitnessParams{..} = do
        wrLogging <- allocResource wpLoggingParams
        wrDB <- allocResource wpDBParams
        return WitnessResources {..}


-- | Resources used by Witness node

module Disciplina.Witness.Launcher.Resource
       ( WitnessResources (..)
       ) where

import Universum

import System.Wlog (LoggerName)

import Disciplina.DB.Real (NodeDB)
import Disciplina.Launcher.Resource (AllocResource (..))
import Disciplina.Witness.Launcher.Params (WitnessParams (..))

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


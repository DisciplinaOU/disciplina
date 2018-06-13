
-- | Resources used by Witness node

module Disciplina.Witness.Launcher.Resource
       ( WitnessResources (..)
       ) where

import Universum

import Disciplina.DB.Real (NodeDB)
import Disciplina.Launcher.Resource (AllocResource (..), BasicNodeResources (..))
import Disciplina.Witness.Launcher.Params (WitnessParams (..))

-- | Datatype which contains resources required by witness node to start
-- working.
data WitnessResources = WitnessResources
    { wrBasicResources :: !BasicNodeResources
    , wrDB             :: !NodeDB
    }

instance AllocResource WitnessParams WitnessResources where
    allocResource WitnessParams{..} = do
        wrBasicResources <- allocResource wpBasicParams
        wrDB <- allocResource wpDBParams
        return WitnessResources {..}


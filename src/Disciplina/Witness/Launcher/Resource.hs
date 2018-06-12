
-- | Resources used by Witness node

module Disciplina.Witness.Launcher.Resource
       ( WitnessResources (..)
       ) where

import Universum

import Disciplina.DB.Real (NodeDB)
import Disciplina.Launcher.Resource (BasicNodeResources (..), BracketResource (..))
import Disciplina.Witness.Launcher.Params (WitnessParams (..))

-- | Datatype which contains resources required by witness node to start
-- working.
data WitnessResources = WitnessResources
    { wrBasicResources :: !BasicNodeResources
    , wrDB             :: !NodeDB
    }

instance BracketResource WitnessParams WitnessResources where
    bracketResourceC WitnessParams{..} = do
        wrBasicResources <- bracketResourceC wpBasicParams
        wrDB <- bracketResourceC wpDBParams
        return WitnessResources {..}


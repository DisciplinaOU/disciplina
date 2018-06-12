
-- | Resources used by Witness node

module Disciplina.WorldState.Launcher.Resource
       ( WitnessResources (..)
       ) where

import Universum

import Disciplina.DB.Real (NodeDB)
import Disciplina.Launcher.Mode (FormNodeContext (..), NodeContext (..))
import Disciplina.Launcher.Resource (BasicNodeResources (..), BracketResource (..))
import Disciplina.WorldState.Launcher.Params (WitnessParams (..))
import Disciplina.WorldState.Mode (Witness, WitnessCustomContext (..))

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

instance FormNodeContext WitnessResources (NodeContext Witness) where
    formNodeContext WitnessResources{..} =
        pure NodeContext
        { _ncLoggerName = bnrLoggerName wrBasicResources
        , _ncCustomCtx = WitnessCustomContext
            { _wccDB = wrDB
            }
        }

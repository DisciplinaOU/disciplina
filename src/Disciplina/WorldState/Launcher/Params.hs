module Disciplina.WorldState.Launcher.Params
       ( WitnessParams (..)
       ) where

import Universum

import Disciplina.DB.Real.Types (DBParams)
import Disciplina.Launcher.Params (BasicNodeParams)

-- | Contains all initialization parameters of Witness node.
data WitnessParams = WitnessParams
    { wpBasicParams :: !BasicNodeParams
    -- ^ Basic parameters for any node
    , wpDBParams    :: !DBParams
    -- ^ DB parameters
    } deriving Show


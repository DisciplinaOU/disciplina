module Disciplina.Witness.Launcher.Params
       ( WitnessParams (..)
       ) where

import Universum

import Disciplina.DB.Real.Types (DBParams)
import Disciplina.Launcher.Params (LoggingParams)

-- | Contains all initialization parameters of Witness node.
data WitnessParams = WitnessParams
    { wpLoggingParams :: !LoggingParams
    -- ^ Basic parameters for any node
    , wpDBParams      :: !DBParams
    -- ^ DB parameters
    } deriving Show


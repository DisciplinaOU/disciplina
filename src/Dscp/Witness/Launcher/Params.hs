module Dscp.Witness.Launcher.Params
       ( WitnessParams (..)
       ) where

import Universum

import Dscp.DB.Real.Types (DBParams)
import Dscp.Launcher.Params (LoggingParams)

-- | Contains all initialization parameters of Witness node.
data WitnessParams = WitnessParams
    { wpLoggingParams :: !LoggingParams
    -- ^ Basic parameters for any node
    , wpDBParams      :: !DBParams
    -- ^ DB parameters
    } deriving Show


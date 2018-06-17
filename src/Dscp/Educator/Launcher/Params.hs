module Dscp.Educator.Launcher.Params where

import Dscp.Witness.Launcher.Params (WitnessParams)

-- | Contains all initialization parameters of Educator node.
data EducatorParams = EducatorParams
    { epWitnessParams :: !WitnessParams
    -- ^ Witness parameters
    }

module Disciplina.Educator.Launcher.Params where

import Disciplina.Witness.Launcher.Params (WitnessParams)

-- | Contains all initialization parameters of Educator node.
data EducatorParams = EducatorParams
    { epWitnessParams :: !WitnessParams
    -- ^ Witness parameters
    }

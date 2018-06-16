module Dscp.Educator.Launcher.Params
    ( EducatorParams(..)
    ) where

import Dscp.DB.SQLite (SQLiteParams)
import Dscp.Witness.Launcher.Params (WitnessParams)

-- | Contains all initialization parameters of Educator node.
data EducatorParams = EducatorParams
    { epWitnessParams :: !WitnessParams
    -- ^ Witness parameters
    , epDBParams      :: !SQLiteParams
    -- ^ Handler to SQLite database
    }

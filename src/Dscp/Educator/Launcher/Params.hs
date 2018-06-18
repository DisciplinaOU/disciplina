module Dscp.Educator.Launcher.Params where

import Dscp.DB.SQLite (SQLiteParams)
import Dscp.Witness.Launcher.Params (WitnessParams)

-- | Contains all initialization parameters of Educator node.
data EducatorParams = EducatorParams
    { epWitnessParams :: !WitnessParams
    -- ^ Witness parameters
    , epSQLiteParams  :: !SQLiteParams
    -- ^ Handler to SQLite database
    }

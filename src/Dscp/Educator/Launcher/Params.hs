module Dscp.Educator.Launcher.Params
    ( EducatorParams(..)
    ) where

import Dscp.DB.SQLite (SQLiteParams)
import Dscp.Educator.Secret.Real.Types (EducatorSecretParams)
import Dscp.Witness.Launcher.Params (WitnessParams)

-- | Contains all initialization parameters of Educator node.
data EducatorParams = EducatorParams
    { epWitnessParams :: !WitnessParams
    -- ^ Witness parameters
    , epDBParams      :: !SQLiteParams
    -- ^ Parameters for SQLite database
    , epSecretParams  :: !EducatorSecretParams
    -- ^ Parameters for educator secret key storage.
    }

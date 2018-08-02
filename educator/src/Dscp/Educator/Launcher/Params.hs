{-# LANGUAGE StrictData #-}

module Dscp.Educator.Launcher.Params
       ( EducatorParams(..)
       ) where

import Dscp.DB.SQLite (SQLiteParams)
import Dscp.Educator.Web.Params (EducatorWebParams)
import Dscp.Resource.Keys.Types (EducatorKeyParams)
import Dscp.Witness.Launcher.Params (WitnessParams)


-- | Contains all initialization parameters of Educator node.
data EducatorParams = EducatorParams
    { epWitnessParams :: WitnessParams
    -- ^ Witness parameters
    , epDBParams      :: SQLiteParams
    -- ^ Parameters for SQLite database
    , epKeyParams     :: EducatorKeyParams
    -- ^ Parameters for educator secret key storage.
    , epWebParams     :: EducatorWebParams
    -- ^ Parameters for HTTP APIs
    }

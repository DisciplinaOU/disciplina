{-# LANGUAGE StrictData #-}

module Dscp.Educator.Launcher.Params
       ( EducatorKeyParams (..)
       , EducatorParams(..)
       ) where

import Dscp.DB.SQLite (SQLiteParams)
import Dscp.Educator.Web.Params (EducatorWebParams)
import Dscp.Resource.Keys (BaseKeyParams)
import Dscp.Witness.Launcher.Params (WitnessParams)

-- | Educator key parameters.
newtype EducatorKeyParams = EducatorKeyParams
    { unEducatorKeyParams :: BaseKeyParams
    } deriving (Show)

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

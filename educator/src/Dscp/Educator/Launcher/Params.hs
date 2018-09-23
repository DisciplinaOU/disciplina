module Dscp.Educator.Launcher.Params
       ( EducatorKeyParams (..)
       ) where

import Data.Aeson (FromJSON (..))

import Dscp.Resource.Keys (BaseKeyParams)

-- | Educator key parameters.
newtype EducatorKeyParams = EducatorKeyParams
    { unEducatorKeyParams :: BaseKeyParams
    } deriving (Show, FromJSON)

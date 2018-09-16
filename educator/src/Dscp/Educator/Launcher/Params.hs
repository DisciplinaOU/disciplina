{-# LANGUAGE StrictData #-}

module Dscp.Educator.Launcher.Params
       ( EducatorKeyParams (..)
       ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)

import Dscp.Resource.Keys (BaseKeyParams)

-- | Educator key parameters.
newtype EducatorKeyParams = EducatorKeyParams
    { unEducatorKeyParams :: BaseKeyParams
    } deriving (Show)

deriveFromJSON defaultOptions ''EducatorKeyParams

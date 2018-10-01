{-# LANGUAGE StrictData #-}

-- | Types specific to educator API.

module Dscp.MultiEducator.Web.Educator.Types
    ( LoginData (..)
    ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)

-- | Login data sent by client
data LoginData = LoginData
   { ldLogin :: Text
   , ldPassword :: Text
   }

deriveJSON defaultOptions ''LoginData

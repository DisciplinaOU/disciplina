{-# LANGUAGE StrictData #-}

-- | Types specific to educator API.

module Dscp.MultiEducator.Web.Educator.Types
    ( LoginData (..)
    ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)

import Dscp.Crypto

-- | Login data sent by client
data LoginData = LoginData
   { ldLogin    :: Text
   , ldPassword :: Maybe PassPhrase
   }

deriveJSON defaultOptions ''LoginData

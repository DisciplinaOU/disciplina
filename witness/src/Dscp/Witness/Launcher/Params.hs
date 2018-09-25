{-# LANGUAGE StrictData #-}

module Dscp.Witness.Launcher.Params
       ( WitnessKeyParams (..)
       ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)

import Dscp.Resource.Keys (BaseKeyParams, CommitteeParams)

-- | Witness key parameters.
data WitnessKeyParams = WitnessKeyParams
    { wkpBase      :: BaseKeyParams
    , wkpCommittee :: (Maybe CommitteeParams)
      -- ^ Optional committee params which may alter key generation.
    } deriving (Show)

-- | JSON instances (for configuration specs)
deriveFromJSON defaultOptions ''WitnessKeyParams

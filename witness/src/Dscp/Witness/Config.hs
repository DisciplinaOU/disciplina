{-# LANGUAGE TypeOperators #-}

-- | All witness's configurations.

module Dscp.Witness.Config
    ( WitnessConfig
    , WitnessConfigRec
    , HasWitnessConfig
    , withWitnessConfig
    , fillWitnessConfig

    , module Dscp.Core.Config
    ) where

import Data.Reflection (Given)
import Loot.Config (ConfigKind (Final, Partial), ConfigRec)

import Dscp.Config
import Dscp.Core.Config

type WitnessConfig = CoreConfig +++
    '[
     ]

type WitnessConfigRecP = ConfigRec 'Partial WitnessConfig
type WitnessConfigRec = ConfigRec 'Final WitnessConfig

-- TODO: `HasWitnessConfig` does not imply `HasCoreConfig`,
-- even though `CoreConfig` is included into `WitnessConfig`.
-- We need some more advanced constraint, something like
-- @type GivenSub a = forall b. (b <: a, Given b)
type HasWitnessConfig = Given WitnessConfigRec

withWitnessConfig :: WitnessConfigRec -> (HasWitnessConfig => a) -> a
withWitnessConfig = withCoreConfig

fillWitnessConfig :: WitnessConfigRecP -> IO WitnessConfigRecP
fillWitnessConfig = fillCoreConfig

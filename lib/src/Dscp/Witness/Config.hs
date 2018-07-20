-- | All witness's configurations.

module Dscp.Witness.Config
    ( WitnessConfig
    , HasWitnessConfig
    , withWitnessConfig
    ) where

import Dscp.Config (BaseConfig, HasBaseConfig, withBaseConfig)

type WitnessConfig = BaseConfig

type HasWitnessConfig = HasBaseConfig

withWitnessConfig :: WitnessConfig -> (HasWitnessConfig => a) -> a
withWitnessConfig = withBaseConfig

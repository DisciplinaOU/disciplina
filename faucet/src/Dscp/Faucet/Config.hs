-- | All witness's configurations.

module Dscp.Faucet.Config
    ( FaucetConfig
    , FaucetConfigRec
    , HasFaucetConfig
    , withFaucetConfig
    , fillFaucetConfig

    , module Dscp.Witness.Config
    ) where

import Data.Reflection (Given)
import Loot.Config (ConfigKind (Final, Partial), ConfigRec)

import Dscp.Witness.Config

type FaucetConfig = WitnessConfig

type FaucetConfigRecP = ConfigRec 'Partial FaucetConfig
type FaucetConfigRec = ConfigRec 'Final FaucetConfig

type HasFaucetConfig = Given FaucetConfigRec

withFaucetConfig :: FaucetConfigRec -> (HasFaucetConfig => a) -> a
withFaucetConfig = withWitnessConfig

fillFaucetConfig :: FaucetConfigRecP -> IO FaucetConfigRecP
fillFaucetConfig = fillWitnessConfig

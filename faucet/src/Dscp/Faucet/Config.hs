-- | All witness's configurations.

module Dscp.Faucet.Config
    ( FaucetConfig
    , FaucetConfigRec
    , HasFaucetConfig
    , withFaucetConfig
    , fillFaucetConfig

    , module Dscp.Core.Config
    ) where

import Data.Reflection (Given)
import Loot.Config (ConfigKind (Final, Partial), ConfigRec)

import Dscp.Core.Config


type FaucetConfig = CoreConfig

type FaucetConfigRecP = ConfigRec 'Partial FaucetConfig
type FaucetConfigRec = ConfigRec 'Final FaucetConfig

type HasFaucetConfig = Given FaucetConfigRec

withFaucetConfig :: FaucetConfigRec -> (HasFaucetConfig => a) -> a
withFaucetConfig = withCoreConfig

fillFaucetConfig :: FaucetConfigRecP -> IO FaucetConfigRecP
fillFaucetConfig = fillCoreConfig

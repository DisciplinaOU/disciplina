{-# LANGUAGE TypeOperators #-}

-- | All witness's configurations.

module Dscp.Faucet.Config
    ( FaucetConfig
    , FaucetConfigRec
    , HasFaucetConfig
    , TransferredAmount (..)
    , DryRun (..)
    , faucetConfig
    , withFaucetConfig
    , fillFaucetConfig

    , module Dscp.Core.Config
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Reflection (Given (..), give)
import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec)

import Dscp.Config
import Dscp.Core
import Dscp.Core.Config
import Dscp.Resource.AppDir
import Dscp.Resource.Keys
import Dscp.Resource.Logging
import Dscp.Web

-- | How much money would be trasferred on each request.
newtype TransferredAmount = TransferredAmount { getTranferredAmount :: Coin }
    deriving (FromJSON, ToJSON)

-- | Whether need not to communicate with witness backend.
newtype DryRun = DryRun Bool
    deriving (FromJSON, ToJSON)

type FaucetConfig = CoreConfig ++
    '[ "faucet" ::<
       '[ "logging" ::: LoggingParams
          -- ^ Logging params.
        , "keys" ::: BaseKeyParams
          -- ^ Corresponds to source of transactions made by faucet.
        , "api" ::: ServerParams
          -- ^ Parameters of faucet API server.
        , "witnessBackend" ::: BaseUrl
          -- ^ Address of transactions processing backend.
        , "transferredAmount" ::: TransferredAmount
          -- ^ How much money to send on request.
        , "dryRun" ::: DryRun
          -- ^ Do not actually communicate with witness backend.
        , "appDir" ::: AppDirParam
          -- ^ Application directory for witness
        ]
     ]

type FaucetConfigRecP = ConfigRec 'Partial FaucetConfig
type FaucetConfigRec = ConfigRec 'Final FaucetConfig

type HasFaucetConfig = Given FaucetConfigRec

faucetConfig :: HasFaucetConfig => FaucetConfigRec
faucetConfig = given

withFaucetConfig :: FaucetConfigRec -> (HasFaucetConfig => a) -> a
withFaucetConfig = give

fillFaucetConfig :: FaucetConfigRecP -> IO FaucetConfigRecP
fillFaucetConfig = fillExpandedConfig fillCoreConfig

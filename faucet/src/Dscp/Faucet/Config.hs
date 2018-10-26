{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | All witness's configurations.

module Dscp.Faucet.Config
    ( FaucetConfig
    , FaucetConfigRec
    , HasFaucetConfig
    , TransferredAmount (..)
    , DryRun (..)
    , defaultFaucetConfig
    , faucetConfig
    , withFaucetConfig
    , fillFaucetConfig

    , module Dscp.Core.Config
    ) where

import Control.Lens ((?~))
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
    deriving (Show, Eq, FromJSON, ToJSON)

-- | Whether need not to communicate with witness backend.
newtype DryRun = DryRun Bool
    deriving (Show, Eq, FromJSON, ToJSON)

-- | Define config parameters for the Faucet
--    [@logging@] Logging params.
--
--    [@keys@] Corresponds to source of transactions made by faucet.
--
--    [@api@] Parameters of faucet API server.
--
--    [@witnessBackend@] Address of transactions processing backend.
--
--    [@transferredAmount@] How much money to send on request.
--
--    [@dryRun@] Do not actually communicate with witness backend.
--
--    [@appDir@] Application directory for witness.
type FaucetConfig = CoreConfig ++
    '[ "faucet" ::<
       '[ "logging" ::: LoggingParams
        , "keys" ::: BaseKeyParams
        , "api" ::: ServerParams
        , "witnessBackend" ::: BaseUrl
        , "transferredAmount" ::: TransferredAmount
        , "dryRun" ::: DryRun
        , "appDir" ::: AppDirParam
        ]
     ]

type FaucetConfigRecP = ConfigRec 'Partial FaucetConfig
type FaucetConfigRec = ConfigRec 'Final FaucetConfig

type HasFaucetConfig = Given FaucetConfigRec

defaultFaucetConfig :: FaucetConfigRecP
defaultFaucetConfig = mempty
    & sub #faucet . option #logging ?~ basicLoggingParams "faucet" False
    & sub #faucet . option #keys ?~ BaseKeyParams Nothing False Nothing
    & sub #faucet . option #dryRun ?~ DryRun False

faucetConfig :: HasFaucetConfig => FaucetConfigRec
faucetConfig = given

withFaucetConfig :: FaucetConfigRec -> (HasFaucetConfig => a) -> a
withFaucetConfig = give

fillFaucetConfig :: FaucetConfigRecP -> IO FaucetConfigRecP
fillFaucetConfig = fillExpandedConfig fillCoreConfig

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | All wallet's configurations.

module Dscp.Wallet.Config
    ( WalletConfig
    , WalletConfigRec
    , HasWalletConfig
    , defaultWalletConfig
    , walletConfig
    , withWalletConfig
    , fillWalletConfig
    , witnessUrl
    , LoggingParams

    , module Dscp.Core.Config
    ) where

import Control.Lens ((?~))
import Data.Reflection (Given (..), give)
import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec, option, sub)

import Dscp.Config
import Dscp.Core.Config
import Dscp.Resource.Logging (LoggingParams (..), basicLoggingParams)
import Dscp.Web

type WalletConfig = CoreConfig ++
    '[ "wallet" ::<
       '[ "logging" ::: LoggingParams
        , "witness" ::: BaseUrl
        ]
     ]

type WalletConfigRecP = ConfigRec 'Partial WalletConfig
type WalletConfigRec = ConfigRec 'Final WalletConfig

type HasWalletConfig = Given WalletConfigRec

---------------------------------------------------------------------------
-- Config itself
---------------------------------------------------------------------------

defaultWalletConfig :: WalletConfigRecP
defaultWalletConfig = mempty
    & sub #wallet . option #logging ?~ basicLoggingParams "wallet" False

walletConfig :: HasWalletConfig => WalletConfigRec
walletConfig = given

withWalletConfig :: WalletConfigRec -> (HasWalletConfig => a) -> a
withWalletConfig = give

fillWalletConfig :: WalletConfigRecP -> IO WalletConfigRecP
fillWalletConfig = fillExpandedConfig fillCoreConfig

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

witnessUrl :: HasWalletConfig => BaseUrl
witnessUrl = giveL @WalletConfig @BaseUrl

-- knitCommand :: HasWalletConfig => (Maybe Text)
-- knitCommand = giveL @WalletConfig @(Maybe Text)

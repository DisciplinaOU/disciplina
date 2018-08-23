{-# LANGUAGE StrictData #-}

module Dscp.Faucet.Launcher.Params
    ( TranslatedAmount (..)
    , FaucetParams (..)
    , DryRun (..)
    , fpLoggingParams
    , fpKeyParams
    , fpWebParams
    , fpWitnessAddress
    , fpTranslatedAmount
    , fpDryRun
    ) where

import Control.Lens (makeLenses)

import Dscp.Core
import Dscp.Resource.Keys
import Dscp.Resource.Logging
import Dscp.Web

-- | How much money would be traslated on each request.
newtype TranslatedAmount = TranslatedAmount { getTranslatedAmount :: Coin }

-- | Whether need not to communicate with witness backend.
newtype DryRun = DryRun Bool

data FaucetParams = FaucetParams
    { _fpLoggingParams    :: LoggingParams
      -- ^ Logging params.
    , _fpKeyParams        :: BaseKeyParams
      -- ^ Corresponds to source of transactions made by faucet.
    , _fpWebParams        :: ServerParams
      -- ^ Parameters of faucet API server.
    , _fpWitnessAddress   :: BaseUrl
      -- ^ Address of transactions processing backend.
    , _fpTranslatedAmount :: TranslatedAmount
      -- ^ How much money to send on request.
    , _fpDryRun           :: DryRun
      -- ^ Do not actually communicate with witness backend.
    }

makeLenses ''FaucetParams

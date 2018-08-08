{-# LANGUAGE StrictData #-}

module Dscp.Faucet.Launcher.Params
    ( TranslatedAmount (..)
    , FaucetParams (..)
    , fpLoggingParams
    , fpKeyParams
    , fpWebParams
    , fpWitnessAddress
    , fpTranslatedAmount
    ) where

import Control.Lens (makeLenses)

import Dscp.Core
import Dscp.Resource.Keys
import Dscp.Resource.Logging
import Dscp.Web

-- | How much money would be traslated on each request.
newtype TranslatedAmount = TranslatedAmount { getTranslatedAmount :: Coin }

data FaucetParams = FaucetParams
    { _fpLoggingParams    :: LoggingParams
      -- ^ Logging params.
    , _fpKeyParams        :: BaseKeyParams
      -- ^ Corresponds to source of transactions made by faucet.
    , _fpWebParams        :: ServerParams
      -- ^ Parameters of faucet API server.
    , _fpWitnessAddress   :: NetworkAddress
      -- ^ Address of transactions processing backend.
    , _fpTranslatedAmount :: TranslatedAmount
      -- ^ How much money to send on request.
    }

makeLenses ''FaucetParams

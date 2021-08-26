module Dscp.Faucet.Launcher.Marker
    ( FaucetApp
    ) where

import qualified Data.Text.Buildable

-- | Indicator of faucet.
data FaucetApp

instance Buildable (Proxy FaucetApp) where
    build _ = "faucet"

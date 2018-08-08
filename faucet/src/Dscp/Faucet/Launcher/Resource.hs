{-# LANGUAGE StrictData #-}

module Dscp.Faucet.Launcher.Resource
    ( FaucetResources (..)
    , frLogging
    , frKeys
    , frWitnessClient
    ) where

import Control.Lens (makeLenses)
import Loot.Base.HasLens (lensOf)
import Loot.Log (Logging)

import Dscp.Faucet.Config
import Dscp.Faucet.Launcher.Marker
import Dscp.Faucet.Launcher.Params
import Dscp.Resource.Class
import Dscp.Resource.Keys
import Dscp.Witness.Web.Client

data FaucetResources = FaucetResources
    { _frLogging       :: Logging IO
    , _frKeys          :: KeyResources FaucetApp
    , _frWitnessClient :: WitnessClient
    }

makeLenses ''FaucetResources

instance HasFaucetConfig =>
         AllocResource BaseKeyParams (KeyResources FaucetApp) where
    allocResource baseParams =
        buildComponentR "faucet keys"
            (linkStore baseParams Nothing)
            (\_ -> pass)

instance HasFaucetConfig =>
         AllocResource FaucetParams FaucetResources where
    allocResource FaucetParams{..} = do
        _frLogging <- view (lensOf @(Logging IO))
        _frKeys <- allocResource _fpKeyParams
        _frWitnessClient <- allocResource _fpWitnessAddress
        return FaucetResources{..}

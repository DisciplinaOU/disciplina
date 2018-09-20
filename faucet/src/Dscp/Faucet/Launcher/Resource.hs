{-# LANGUAGE StrictData #-}

module Dscp.Faucet.Launcher.Resource
    ( FaucetResources (..)
    , frLogging
    , frKeys
    , frWitnessClient
    , frAppDir
    ) where

import Control.Lens (makeLenses)
import Loot.Base.HasLens (lensOf)
import Loot.Log (Logging)

import Dscp.Config (rcast)
import Dscp.Faucet.Config
import Dscp.Faucet.Launcher.Marker
import Dscp.Faucet.Launcher.Params
import Dscp.Resource.AppDir
import Dscp.Resource.Class
import Dscp.Resource.Keys
import Dscp.Witness.Web.Client

data FaucetResources = FaucetResources
    { _frLogging       :: Logging IO
    , _frKeys          :: KeyResources FaucetApp
    , _frWitnessClient :: WitnessClient
    , _frAppDir        :: AppDir
    }

makeLenses ''FaucetResources

instance AllocResource (KeyResources FaucetApp) where
    type Deps (KeyResources FaucetApp) =
        (WitnessConfigRec, BaseKeyParams, AppDir)

    allocResource (witnessCfg, baseParams, appDir) =
        buildComponentR "faucet keys"
            (withWitnessConfig witnessCfg $ linkStore baseParams Nothing appDir)
            (\_ -> pass)

instance AllocResource FaucetResources where
    type Deps FaucetResources = (FaucetConfigRec, FaucetParams)
    allocResource (faucetCfg, FaucetParams{..}) = do
        _frLogging <- view (lensOf @(Logging IO))
        _frAppDir <- allocResource _fpAppDirParam
        _frKeys <- allocResource (rcast faucetCfg, _fpKeyParams, _frAppDir)
        _frWitnessClient <- allocResource _fpWitnessAddress
        return FaucetResources{..}

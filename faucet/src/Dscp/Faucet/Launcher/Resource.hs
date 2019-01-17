{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData       #-}

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

import Dscp.Config
import Dscp.Faucet.Config
import Dscp.Faucet.Launcher.Marker
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
    type Deps (KeyResources FaucetApp) = (FaucetConfigRec, AppDir)
    allocResource (faucetCfg, appDir) =
        let baseParams = faucetCfg ^. sub #faucet . sub #keys
        in buildComponentR "faucet keys"
           (withCoreConfig (rcast faucetCfg) $
               linkStore baseParams appDir)
           (const pass)

instance AllocResource FaucetResources where
    type Deps FaucetResources = FaucetConfigRec
    allocResource faucetCfg = do
        let cfg = faucetCfg ^. sub #faucet
        _frLogging <- view (lensOf @(Logging IO))
        _frAppDir <- allocResource $ cfg ^. sub #appDir
        _frKeys <- allocResource (faucetCfg, _frAppDir)
        _frWitnessClient <- allocResource $ cfg ^. option #witnessBackend
        return FaucetResources{..}

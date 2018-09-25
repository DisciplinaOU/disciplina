{-# LANGUAGE OverloadedLabels #-}

-- | Resources used by Witness node

module Dscp.Witness.Launcher.Resource
       ( WitnessResources (..)
       , wrLogging
       , wrDB
       , wrNetwork
       , wrKey
       , wrAppDir
       ) where

import Loot.Log.Internal (NameSelector (GivenName), logNameSelL)
import Loot.Log.Rio (LoggingIO)

import Control.Lens (makeLenses)
import Loot.Base.HasLens (HasLens (..))

import Dscp.Config
import Dscp.DB.Rocks.Real (RocksDB)
import Dscp.Resource.AppDir
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.Resource.Keys (KeyResources (..), linkStore)
import Dscp.Resource.Network (NetLogging (..), NetServResources, withNetLogging)
import Dscp.Resource.Rocks ()
import Dscp.Util.HasLens
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Marker (WitnessNode)
import Dscp.Witness.Launcher.Params (WitnessKeyParams (..))

-- | Datatype which contains resources required by witness node to start
-- working.
data WitnessResources = WitnessResources
    { _wrLogging :: !LoggingIO
    , _wrDB      :: !RocksDB
    , _wrNetwork :: !NetServResources
    , _wrKey     :: !(KeyResources WitnessNode)
    , _wrAppDir  :: !AppDir
    }

makeLenses ''WitnessResources

deriveHasLensDirect ''WitnessResources
deriveHasLens 'wrNetwork ''WitnessResources ''NetServResources

instance AllocResource (KeyResources WitnessNode) where
    type Deps (KeyResources WitnessNode) = (WitnessConfigRec, AppDir)
    allocResource (witnessCfg, appDir) =
        let WitnessKeyParams {..} = witnessCfg ^. sub #witness . option #keys
        in buildComponentR "witness keys"
           (withCoreConfig (rcast witnessCfg) $
               linkStore wkpBase wkpCommittee appDir)
           (\_ -> pass)

instance AllocResource WitnessResources where
    type Deps WitnessResources = WitnessConfigRec
    allocResource witnessCfg = do
        let cfg = witnessCfg ^. sub #witness
        _wrLogging <- view (lensOf @LoggingIO)
        _wrDB <- allocResource $ cfg ^. option #db
        _wrNetwork <- do
            let modGivenName (GivenName x) = GivenName $ x <> "network"
                modGivenName x             = x
            withNetLogging (NetLogging $ _wrLogging & logNameSelL %~ modGivenName)
                           (allocResource $ cfg ^. option #network)
        _wrAppDir <- allocResource $ cfg ^. option #appDir
        _wrKey <- allocResource (witnessCfg, _wrAppDir)
        return WitnessResources {..}

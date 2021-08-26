{-# LANGUAGE OverloadedLabels #-}

-- | Resources used by Witness node

module Dscp.Witness.Launcher.Resource
       ( WitnessResources (..)
       , wrLogging
       , wrDB
       , wrNetwork
       , wrKey
       , wrAppDir
       , WitnessNode
       ) where

import Loot.Log (MonadLogging)
import Loot.Log.Internal (NameSelector (GivenName), logNameSelL)
import Loot.Log.Rio (LoggingIO)

import Control.Lens (makeLenses)
import Loot.Base.HasLens (HasLens (..))

import Dscp.Config
import Dscp.DB.CanProvideDB as Rocks
import Dscp.DB.CanProvideDB.Rocks as RealRocks
import Dscp.Resource.AppDir
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.Resource.Keys
import Dscp.Resource.Network (NetLogging (..), NetServResources, withNetLogging)
import Dscp.Resource.Rocks ()
import Dscp.Util.HasLens
import Dscp.Witness.Config
import Dscp.Witness.Keys
import Dscp.Witness.Launcher.Marker (WitnessNode)

-- | Datatype which contains resources required by witness node to start
-- working.
data WitnessResources = WitnessResources
    { _wrLogging :: !LoggingIO
    , _wrDB      :: !Rocks.Plugin
    , _wrNetwork :: !NetServResources
    , _wrKey     :: !(KeyResources WitnessNode)
    , _wrAppDir  :: !AppDir
    }

makeLenses ''WitnessResources

deriveHasLensDirect ''WitnessResources
deriveHasLens 'wrNetwork ''WitnessResources ''NetServResources

-- | If 'Basic' key params are used, open or create a keyfile.
-- Otherwise, just generate a key using committee params.
getWitnessKeyResources
    :: (HasCoreConfig, MonadCatch m, MonadIO m, MonadLogging m)
    => WitnessKeyParamsRec
    -> AppDir
    -> m (KeyResources WitnessNode)
getWitnessKeyResources keyParams appDir = case keyParams ^. tree #params . selection of
    "basic"     -> linkStore basicBranch appDir
    "committee" -> mkCommitteeStore committeeBranch
    sel -> error $ "unknown WitnessParams type: " <> fromString sel
  where
    basicBranch = keyParams ^. tree #params . peekBranch #basic
    committeeBranch = keyParams ^. tree #params . peekBranch #committee

instance AllocResource (KeyResources WitnessNode) where
    type Deps (KeyResources WitnessNode) = (WitnessConfigRec, AppDir)
    allocResource (witnessCfg, appDir) =
        let keyParams = witnessCfg ^. sub #witness . sub #keys
        in buildComponentR "witness keys"
           (withCoreConfig (rcast witnessCfg) $
               getWitnessKeyResources keyParams appDir)
           (const pass)

instance AllocResource WitnessResources where
    type Deps WitnessResources = WitnessConfigRec
    allocResource witnessCfg = do
        let cfg = witnessCfg ^. sub #witness
        _wrLogging <- view (lensOf @LoggingIO)
        _wrDB <- fmap RealRocks.plugin . allocResource $ cfg ^. sub #db
        _wrNetwork <- do
            let modGivenName (GivenName x) = GivenName $ x <> "network"
                modGivenName x             = x
            withNetLogging (NetLogging $ _wrLogging & logNameSelL %~ modGivenName)
                           (allocResource $ cfg ^. sub #network)
        _wrAppDir <- allocResource $ cfg ^. sub #appDir
        _wrKey <- allocResource (witnessCfg, _wrAppDir)
        return WitnessResources {..}

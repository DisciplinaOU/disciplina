{-# LANGUAGE OverloadedLabels #-}

-- | Resources used by Educator node

module Dscp.Educator.Launcher.Resource
       ( EducatorResources (..)
       , erWitnessResources
       ) where

import Control.Lens (makeLenses)
import Loot.Base.HasLens (HasLens (..))
import Loot.Config (option, sub)
import Loot.Log.Rio (LoggingIO)
import Loot.Network.ZMQ (ZTGlobalEnv, ZTNetCliEnv, ZTNetServEnv)

import Dscp.Config
import Dscp.DB.Rocks.Real.Types (RocksDB)
import Dscp.DB.SQLite (SQLiteDB)
import Dscp.Educator.Config
import Dscp.Educator.Launcher.Marker (EducatorNode)
import Dscp.Educator.Launcher.Params (EducatorKeyParams (..))
import Dscp.Resource.AppDir
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.Resource.Keys (KeyResources (..), linkStore)
import Dscp.Resource.SQLite ()
import qualified Dscp.Witness.Launcher.Resource as Witness

-- SQL resource should be here too (in the future).
-- | Datatype which contains resources required by all Disciplina nodes
-- to start working.
data EducatorResources = EducatorResources
    { _erWitnessResources :: !Witness.WitnessResources
    , _erDB               :: !SQLiteDB
    , _erKeys             :: !(KeyResources EducatorNode)
    }

makeLenses ''EducatorResources

instance HasLens SQLiteDB EducatorResources SQLiteDB where
    lensOf = erDB
instance HasLens (KeyResources EducatorNode) EducatorResources (KeyResources EducatorNode) where
    lensOf = erKeys
instance HasLens LoggingIO EducatorResources LoggingIO where
    lensOf = erWitnessResources . lensOf @LoggingIO
instance HasLens RocksDB EducatorResources RocksDB where
    lensOf = erWitnessResources . lensOf @RocksDB
instance HasLens ZTGlobalEnv EducatorResources ZTGlobalEnv where
    lensOf = erWitnessResources . lensOf @ZTGlobalEnv
instance HasLens ZTNetCliEnv EducatorResources ZTNetCliEnv where
    lensOf = erWitnessResources . lensOf @ZTNetCliEnv
instance HasLens ZTNetServEnv EducatorResources ZTNetServEnv where
    lensOf = erWitnessResources . lensOf @ZTNetServEnv

instance AllocResource (KeyResources EducatorNode) where
    type Deps (KeyResources EducatorNode) =
        (WitnessConfigRec, EducatorKeyParams, AppDir)

    allocResource (witnessCfg, EducatorKeyParams baseParams, appDir) =
        buildComponentR "educator keys"
            (withWitnessConfig witnessCfg $ linkStore baseParams Nothing appDir)
            (\_ -> pass)

instance AllocResource EducatorResources where
    type Deps EducatorResources = EducatorConfigRec
    allocResource educatorCfg = do
        let cfg = educatorCfg ^. sub #educator
            witnessCfg = rcast educatorCfg
        _erWitnessResources <- withWitnessConfig witnessCfg $
                               allocResource witnessCfg
        _erDB <- allocResource $ cfg ^. option #db
        let appDir = Witness._wrAppDir _erWitnessResources
        _erKeys <- allocResource (witnessCfg, cfg ^. option #keys, appDir)
        return EducatorResources {..}

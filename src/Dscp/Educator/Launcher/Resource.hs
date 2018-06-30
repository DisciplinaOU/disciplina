-- | Resources used by Educator node

module Dscp.Educator.Launcher.Resource
       ( EducatorResources (..)
       , erWitnessResources
       ) where

import Control.Lens (makeLenses)
import Loot.Base.HasLens (HasLens (..))
import Loot.Log.Rio (LoggingIO)
import Loot.Network.ZMQ (ZTGlobalEnv, ZTNetCliEnv, ZTNetServEnv)

import Dscp.DB.Rocks.Real.Types (RocksDB)
import Dscp.DB.SQLite (SQLiteDB)
import Dscp.Educator.Launcher.Params (EducatorParams (..))
import Dscp.Educator.Secret.Types (EducatorSecret)
import Dscp.Resource (AllocResource (..))
import qualified Dscp.Witness.Launcher.Resource as Witness

-- SQL resource should be here too (in the future).
-- | Datatype which contains resources required by all Disciplina nodes
-- to start working.
data EducatorResources = EducatorResources
    { _erWitnessResources :: !Witness.WitnessResources
    , _erDB               :: !SQLiteDB
    , _erSecret           :: !EducatorSecret
    }

makeLenses ''EducatorResources

instance HasLens SQLiteDB EducatorResources SQLiteDB where
    lensOf = erDB
instance HasLens EducatorSecret EducatorResources EducatorSecret where
    lensOf = erSecret
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

instance AllocResource EducatorParams EducatorResources where
    allocResource EducatorParams{..} = do
        _erWitnessResources <- allocResource epWitnessParams
        _erDB <- allocResource epDBParams
        _erSecret <- allocResource epSecretParams
        return EducatorResources {..}

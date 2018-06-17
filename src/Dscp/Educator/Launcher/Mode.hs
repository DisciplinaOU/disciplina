{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of Educator's WorkMode and its implementations.

module Dscp.Educator.Launcher.Mode
    (
      -- * Constraints
      EducatorWorkMode
    , CombinedWorkMode

      -- * Implementations
    , EducatorContext (..)
    , EducatorRealMode
    , ecWitnessCtx
    , ecDB
    ) where

import Universum

import Control.Lens (makeLenses)
import Loot.Base.HasLens (HasLens (..))
import Loot.Log.Rio (LoggingIO)
import Loot.Network.ZMQ as Z

import Dscp.DB.Rocks.Real.Types (RocksDB)
import Dscp.DB.SQLite (MonadSQLiteDB, SQLiteDB)
import qualified Dscp.Launcher.Mode as Basic
import Dscp.Launcher.Rio (RIO)
import qualified Dscp.Witness.Launcher as Witness

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of bare Educator node.
type EducatorWorkMode m =
    ( Basic.BasicWorkMode m
    , MonadSQLiteDB m
    )

-- | Set of typeclasses which define capabilities both of Educator and Witness.
type CombinedWorkMode m =
    ( EducatorWorkMode m
    , Witness.WitnessWorkMode m
    )

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

data EducatorContext = EducatorContext
    { _ecWitnessCtx :: Witness.WitnessContext
    , _ecDB         :: SQLiteDB
    }

makeLenses ''EducatorContext

type EducatorRealMode = RIO EducatorContext

---------------------------------------------------------------------
-- HasLens
---------------------------------------------------------------------

instance HasLens LoggingIO EducatorContext LoggingIO where
    lensOf = ecWitnessCtx . lensOf @LoggingIO
instance HasLens RocksDB EducatorContext RocksDB where
    lensOf = ecWitnessCtx . lensOf @RocksDB
instance HasLens SQLiteDB EducatorContext SQLiteDB where
    lensOf = ecDB
instance HasLens Z.ZTGlobalEnv EducatorContext Z.ZTGlobalEnv where
    lensOf = ecWitnessCtx . lensOf @Z.ZTGlobalEnv
instance HasLens Z.ZTNetCliEnv EducatorContext Z.ZTNetCliEnv where
    lensOf = ecWitnessCtx . lensOf @Z.ZTNetCliEnv
instance HasLens Z.ZTNetServEnv EducatorContext Z.ZTNetServEnv where
    lensOf = ecWitnessCtx . lensOf @Z.ZTNetServEnv

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

_sanity :: EducatorRealMode ()
_sanity = _sanityCallee
  where
    _sanityCallee :: CombinedWorkMode m => m ()
    _sanityCallee = pass

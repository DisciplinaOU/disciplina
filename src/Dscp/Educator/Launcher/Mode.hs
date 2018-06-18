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
import Loot.Log.Rio (LoggingIO)

import Dscp.DB.Rocks.Real.Types (RocksDB)
import Dscp.DB.SQLite (MonadSQLiteDB, SQLiteDB)
import qualified Dscp.Launcher.Mode as Basic
import qualified Dscp.Witness.Launcher as Witness
import Ether.Internal (HasLens (..))

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

type EducatorRealMode = Basic.RIO EducatorContext

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

instance HasLens RocksDB EducatorContext RocksDB where
    lensOf = ecWitnessCtx . Witness.wcDB

instance HasLens LoggingIO EducatorContext LoggingIO where
    lensOf = ecWitnessCtx . Witness.wcLogging

instance HasLens SQLiteDB EducatorContext SQLiteDB where
    lensOf = ecDB

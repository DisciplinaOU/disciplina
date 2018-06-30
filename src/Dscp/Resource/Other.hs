-- | "AllocResource" instances for things that already have parameters
-- datatype and resource datatype.

module Dscp.Resource.Other () where

import Control.Monad.Component (buildComponent)

import Dscp.DB.SQLite (ensureSchemaIsSetUp)
import Dscp.DB.Rocks.Real (RocksDB, RocksDBParams, closeNodeDB, openNodeDB)
import Dscp.DB.SQLite (SQLiteDB (..), SQLiteParams, closeSQLiteDB, openSQLiteDB)
import Dscp.Resource.Class (AllocResource (..))

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance AllocResource RocksDBParams RocksDB where
    allocResource p = buildComponent "RocksDB" (openNodeDB p) closeNodeDB

instance AllocResource SQLiteParams SQLiteDB where
    allocResource p = buildComponent "SQLite DB" (openSQLiteDB' p) closeSQLiteDB
      where
        openSQLiteDB' p' = do
            db@ (SQLiteDB conn) <- openSQLiteDB p'
            ensureSchemaIsSetUp conn
            return db

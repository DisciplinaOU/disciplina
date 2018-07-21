-- | "AllocResource" instances for things that already have parameters
-- datatype and resource datatype.

module Dscp.Resource.Other () where

import Dscp.DB.Rocks.Real (RocksDB, RocksDBParams, closeNodeDB, openNodeDB)
import Dscp.DB.SQLite (SQLiteDB (..), SQLiteParams, closeSQLiteDB, ensureSchemaIsSetUp,
                       openSQLiteDB)
import Dscp.Resource.Class (AllocResource (..), buildComponentR)

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance AllocResource RocksDBParams RocksDB where
    allocResource p = buildComponentR "RocksDB" (openNodeDB p) closeNodeDB

instance AllocResource SQLiteParams SQLiteDB where
    allocResource p = buildComponentR "SQLite DB" (openSQLiteDB' p) closeSQLiteDB
      where
        openSQLiteDB' p' = do
            db@ (SQLiteDB conn) <- openSQLiteDB p'
            ensureSchemaIsSetUp conn
            return db

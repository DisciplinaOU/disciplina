-- | "AllocResource" instances for things that already have parameters
-- datatype and resource datatype.

module Dscp.Resource.Other () where


import Dscp.DB.Rocks.Real (RocksDB, RocksDBParams, closeNodeDB, openNodeDB)
import Dscp.DB.SQLite (ensureSchemaIsSetUp)
import Dscp.DB.SQLite (SQLiteDB (..), SQLiteParams, closeSQLiteDB, openSQLiteDB)
import Dscp.Educator.Secret (EducatorSecret, EducatorSecretParams, linkStore)
import Dscp.Resource.AppDir (AppDirectory)
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

instance AllocResource (EducatorSecretParams, AppDirectory) EducatorSecret where
    allocResource (params, appDir) =
        buildComponentR "Educator secret key storage"
            (linkStore params appDir)
            (\_ -> pass)

-- | "AllocResource" instances for things that already have parameters
-- datatype and resource datatype.

module Dscp.Resource.SQLite () where

import Dscp.DB.SQLite (ensureSchemaIsSetUp)
import Dscp.DB.SQLite (SQLiteDB (..), SQLiteParams, closeSQLiteDB, openSQLiteDB)
import Dscp.Resource.Class (AllocResource (..), buildComponentR)

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance AllocResource SQLiteParams SQLiteDB where
    allocResource p = buildComponentR "SQLite DB" (openSQLiteDB' p) closeSQLiteDB
      where
        openSQLiteDB' p' = do
            db@ (SQLiteDB conn) <- openSQLiteDB p'
            ensureSchemaIsSetUp conn
            return db

-- | "AllocResource" instances for things that already have parameters
-- datatype and resource datatype.

module Dscp.Resource.SQLite
    ( prepareEducatorSchema
    ) where

import Dscp.DB.SQLite (ensureSchemaIsSetUp)
import Dscp.DB.SQLite (SQLiteDB (..), SQLiteParams, closeSQLiteDB, openSQLiteDB)
import Dscp.Resource.Class (AllocResource (..), buildComponentR)

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

prepareEducatorSchema :: MonadIO m => SQLiteDB -> m ()
prepareEducatorSchema db = do
    forEachConnection db applySchemaSettings
    runRIO db $ borrowConnection ensureSchemaIsSetUp

instance AllocResource SQLiteParams SQLiteDB where
    allocResource p = buildComponentR "SQLite DB" (openSQLiteDB' p) closeSQLiteDB
      where
        openSQLiteDB' p' = do
            db <- openSQLiteDB p'
            prepareEducatorSchema db
            return db

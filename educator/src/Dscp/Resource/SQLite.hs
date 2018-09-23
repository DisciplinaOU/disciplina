-- | "AllocResource" instances for things that already have parameters
-- datatype and resource datatype.

module Dscp.Resource.SQLite
    ( prepareEducatorSchema
    ) where

import Dscp.DB.SQLite
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.Rio

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

prepareEducatorSchema :: MonadIO m => SQLiteDB -> m ()
prepareEducatorSchema db = do
    forEachConnection db applySchemaSettings
    runRIO db $ borrowConnection ensureSchemaIsSetUp

instance AllocResource SQLiteDB where
    type Deps SQLiteDB = SQLiteParams
    allocResource p = buildComponentR "SQLite DB" (openSQLiteDB' p) closeSQLiteDB
      where
        openSQLiteDB' p' = do
            db <- openSQLiteDB p'
            prepareEducatorSchema db
            return db

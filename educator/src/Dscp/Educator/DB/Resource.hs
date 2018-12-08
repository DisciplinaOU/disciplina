-- | "AllocResource" instances for things that already have parameters
-- datatype and resource datatype.

module Dscp.Educator.DB.Resource
    ( prepareEducatorSchema
    ) where

import UnliftIO (MonadUnliftIO)

import Dscp.DB.SQLite
import Dscp.Educator.DB.Schema
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.Rio

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

prepareEducatorSchema :: MonadUnliftIO m => SQL -> m ()
prepareEducatorSchema db = do
    -- forEachConnection db applySchemaSettings  TODO
    runRIO db $ transact ensureSchemaIsSetUp

instance AllocResource SQL where
    type Deps SQL = PostgresParams
    allocResource p = buildComponentR "SQLite DB" (openPostgresDB' p) closePostgresDB
      where
        openPostgresDB' p' = do
            db <- openPostgresDB p'
            prepareEducatorSchema db
            return db

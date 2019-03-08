-- | "AllocResource" instances for things that already have parameters
-- datatype and resource datatype.

module Dscp.Educator.DB.Resource
    ( prepareEducatorSchema
    ) where

import Loot.Base.HasLens (HasCtx, lensOf)
import Loot.Log (LoggingIO)
import UnliftIO (MonadUnliftIO)

import Dscp.DB.SQL
import Dscp.Educator.DB.Schema
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.Rio

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

prepareEducatorSchema
    :: (MonadUnliftIO m, HasCtx ctx m '[LoggingIO], HasCallStack)
    => SQL -> m ()
prepareEducatorSchema db = do
    logging <- view (lensOf @LoggingIO)
    runRIO (db, logging) . transact $ ensureSchemaIsSetUp

instance AllocResource (PreparedSQL "educator") where
    type Deps (PreparedSQL "educator") = PostgresRealParamsRec
    allocResource p =
        PreparedSQL <$>
        buildComponentR "SQL DB" (openPostgresDB' p) closePostgresDB
      where
        openPostgresDB' p' = do
            db <- openPostgresDB (PostgresParams $ PostgresReal p')
            prepareEducatorSchema db
            return db

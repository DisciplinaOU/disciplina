module Dscp.DB.SQLite.Class where

import Database.SQLite.Simple (FromRow, Query, ToRow)

-- There are more functions in that library, feel free
-- to add them to typeclass below if there are needed.

-- | Full interface to SQLite DB.
class Monad m => MonadSQLiteDB m where
    -- | Make a simple @INSERT@ or other SQL query.
    query :: (FromRow row, ToRow params) => Query -> params -> m [row]

    -- | Make an @INSERT@ SQL query, feeding result to given function on per-row
    -- basis.
    queryStreamed
        :: (FromRow row, ToRow params)
        => Query -> params -> a -> (a -> row -> m a) -> m a

    -- | Perform a simple SQL query which does not return any result.
    execute :: ToRow q => Query -> q -> m ()

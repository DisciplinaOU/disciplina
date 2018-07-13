module Dscp.DB.SQLite.Class
    ( MonadSQLiteDB (..)

      -- * Nested transactions
    , WithinSQLTransaction
    , sqlTransaction
    ) where

import Data.Reflection (Given, give)
import Database.SQLite.Simple (FromRow, Query, ToRow)

-- There are more functions in that library, feel free
-- to add them to typeclass below if there are needed.

-- | Full interface to SQLite DB.
class MonadIO m => MonadSQLiteDB m where
    -- | Make a simple @INSERT@ or other SQL query.
    query :: (FromRow row, ToRow params) => Query -> params -> m [row]

    -- | Make an @INSERT@ SQL query, feeding result to given function on per-row
    -- basis.
    queryStreamed
        :: (FromRow row, ToRow params)
        => Query -> params -> a -> (a -> row -> m a) -> m a

    -- | Perform a simple SQL query which does not return any result.
    execute :: ToRow q => Query -> q -> m ()

    -- | Submits 'begin/end transaction' commands.
    --
    -- Nested calls seem to be supported only by rather high versions of sqlite,
    -- my engine throws error on attempt to use those (@martoon). Thus, when
    -- need nested transactions require 'WithinSQLTransaction' for evaluation
    -- which should perform atomically instead of calling 'transaction',
    -- and then wrap all SQL operations with 'sqlTransaction'.
    transaction :: m a -> m a

----------------------------------------------------------
-- Nested transactions
----------------------------------------------------------

data SQLTransactionContext = SQLTransactionContext
-- | Requre this if you want to perform commands atomically but can not use
-- 'transaction' directly in order to allow a function to be nested in another
-- function with query.
type WithinSQLTransaction = Given SQLTransactionContext

-- | Used to call functions with 'WithSQLTransaction' costraint.
sqlTransaction :: MonadSQLiteDB m => (WithinSQLTransaction => m a) -> m a
sqlTransaction act = transaction $ give SQLTransactionContext act

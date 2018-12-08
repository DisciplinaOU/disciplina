-- | Exceptions happening during work with SQLLite.

module Dscp.DB.SQLite.Error
    ( SQLConnectionOpenningError (..)
    , SQLRequestsNumberExceeded (..)

      -- * Common SQL errors
    , asAlreadyExistsError
    , asReferenceInvalidError
    ) where

import qualified Data.Text.Buildable
import Database.PostgreSQL.Simple (SqlError)
import Database.PostgreSQL.Simple.Errors (ConstraintViolation (..), constraintViolation)
import Fmt ((+|), (|+))
import qualified Text.Show

-- | All errors which may happen on DB openning.
data SQLConnectionOpenningError
    = SQLInvalidPathError !FilePath
      -- ^ Used 'SQLiteReal' constructor with bad filepath
    | SQLInvalidConnectionsNumber !Int
      -- ^ Bad number of connections passed
    | SQLInvalidMaxPendingNumber !Int
      -- ^ Bad number of max pending threads
    | SQLConnectionOpenningError !Text
      -- ^ Exception in SQL backend, doc doesn't specify which one

instance Show SQLConnectionOpenningError where
    show = toString . pretty

instance Buildable SQLConnectionOpenningError where
    build = \case
        SQLInvalidPathError path ->
            "Given unusable path for sqlite database: "+|path|+""
        SQLInvalidConnectionsNumber num ->
            "Illegal number of connections: "+|num|+""
        SQLInvalidMaxPendingNumber num ->
            "Illegal number of maximum pending threads: "+|num|+""
        SQLConnectionOpenningError msg ->
            "Failed to open database connection: "+|msg|+""

instance Exception SQLConnectionOpenningError

-- | Maximum allowed number of pending threads is exceeded.
data SQLRequestsNumberExceeded = SQLRequestsNumberExceeded

instance Show SQLRequestsNumberExceeded where
    show = toString . pretty

instance Buildable SQLRequestsNumberExceeded where
    build _ = "Too many requests to SQLite database"

instance Exception SQLRequestsNumberExceeded

----------------------------------------------------------
-- Common SQL errors
----------------------------------------------------------

-- | Matches on errors declaring violation of UNIQUE constraint,
-- returns name of fields on which constraint was violated.
asAlreadyExistsError :: SqlError -> Maybe ByteString
asAlreadyExistsError err = do
    err' <- constraintViolation err
    UniqueViolation constr <- pure err'
    return constr

-- | Matches on errors which declare violation of FOREIGN KEY constraint,
-- returns descrition of violated constraint.
asReferenceInvalidError :: SqlError -> Maybe (ByteString, ByteString)
asReferenceInvalidError err = do
    err' <- constraintViolation err
    ForeignKeyViolation tbl constr <- pure err'
    return (tbl, constr)

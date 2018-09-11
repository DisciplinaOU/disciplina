-- | Exceptions happening during work with SQLLite.

module Dscp.DB.SQLite.Error
    ( SQLConnectionOpenningError (..)
    , SQLRequestsNumberExceeded (..)

      -- * Common SQL errors
    , asAlreadyExistsError
    , asReferenceInvalidError
    ) where

import qualified Data.Text as T
import qualified Data.Text.Buildable
import Database.SQLite.Simple (Error (..), SQLError (..))
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
asAlreadyExistsError :: SQLError -> Maybe Text
asAlreadyExistsError err = do
    SQLError ErrorConstraint details _ <- pure err
    let pat = "UNIQUE constraint failed"
    guard $ pat `T.isPrefixOf` details
    return $ T.drop (length pat) details

-- | Matches on errors which declare violation of FOREIGN KEY constraint,
-- returns descrition of violated constraint.
asReferenceInvalidError :: SQLError -> Maybe Text
asReferenceInvalidError err = do
    SQLError ErrorConstraint details _ <- pure err
    let pat = "FOREIGN KEY constraint failed"
    guard $ pat `T.isPrefixOf` details
    return $ T.drop (length pat) details

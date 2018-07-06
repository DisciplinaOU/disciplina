-- | Exceptions happening during work with SQLLite.

module Dscp.DB.SQLite.Error
    ( SQLConnectionOpenningError (..)
    , SQLRequestError (..)
    , rethrowSQLRequestError
    ) where

import qualified Data.Text.Buildable
import Database.SQLite.Simple as SQLite
import Fmt ((+|), (+||), (|+), (||+))
import qualified Text.Show

import Dscp.Util (wrapRethrow)

-- | All errors which may happen on DB openning.
data SQLConnectionOpenningError
    = SQLInvalidPathError !FilePath
      -- ^ Used 'SQLiteReal' constructor with bad filepath
    | SQLConnectionOpenningError !Text
      -- ^ Exception in SQL backend, doc doesn't specify which one

instance Show SQLConnectionOpenningError where
    show = toString . pretty

instance Buildable SQLConnectionOpenningError where
    build = \case
        SQLInvalidPathError path ->
            "Given unusable path for sqlite database: "+|path|+""
        SQLConnectionOpenningError msg ->
            "Failed to open database connection: "+|msg|+""

instance Exception SQLConnectionOpenningError

-- | All errors which may happen on any SQL query.
data SQLRequestError
    = SQLFormatError SQLite.FormatError
      -- ^ Query string mismatched with given parameters
    | SQLResultError SQLite.ResultError
      -- ^ Deserialisation failure

instance Show SQLRequestError where
    show = toString . pretty

instance Buildable SQLRequestError where
    build = \case
        SQLFormatError err -> "Invalid SQL request: "+||err||+""
        SQLResultError err -> "Failed to deserialise result: "+||err||+""

instance Exception SQLRequestError

-- | Wrap possible appropriate exceptions into 'SQLRequestError'.
rethrowSQLRequestError :: MonadCatch m => m a -> m a
rethrowSQLRequestError =
    wrapRethrow SQLFormatError .
    wrapRethrow SQLResultError

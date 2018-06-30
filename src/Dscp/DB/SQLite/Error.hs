-- | Exceptions happening during work with SQLLite.

module Dscp.DB.SQLite.Error
    ( SQLConnectionOpenningError (..)
    , SQLRequestError (..)
    , rethrowSQLRequestError
    ) where

import Database.SQLite.Simple as SQLite
import Dscp.Util (wrapRethrow)

-- | All errors which may happen on DB openning.
data SQLConnectionOpenningError
    = SQLInvalidPathError !FilePath
      -- ^ Used 'SQLiteReal' constructor with bad filepath
    | SQLConnectionOpenningError !Text
      -- ^ Exception in SQL backend, doc doesn't specify which one
    deriving (Show)

instance Exception SQLConnectionOpenningError

-- | All errors which may happen on any SQL query.
data SQLRequestError
    = SQLFormatError SQLite.FormatError
      -- ^ Query string mismatched with given parameters.
    | SQLResultError SQLite.ResultError
      -- ^ Result conversion failed.
    deriving (Show)

instance Exception SQLRequestError

-- | Wrap possible appropriate exceptions into 'SQLRequestError'.
rethrowSQLRequestError :: MonadCatch m => m a -> m a
rethrowSQLRequestError =
    wrapRethrow SQLFormatError .
    wrapRethrow SQLResultError

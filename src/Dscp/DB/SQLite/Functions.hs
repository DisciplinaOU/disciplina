{-# LANGUAGE TypeApplications #-}

module Dscp.DB.SQLite.Functions
       ( -- * Closing/opening
         openSQLiteDB
       , closeSQLiteDB
       ) where

import Universum

import qualified Database.SQLite.Simple as Lower

import Dscp.DB.SQLite.Types (SQLiteDB (..), SQLiteDBLocation (..), SQLiteParams (..))

-----------------------------------------------------------
-- Opening/closing
-----------------------------------------------------------

newtype InvalidSQLitePathException = InvalidSQLitePathException FilePath
    deriving (Show)

instance Exception InvalidSQLitePathException

openSQLiteDB
    :: (MonadIO m, MonadThrow m)
    => SQLiteParams -> m SQLiteDB
openSQLiteDB SQLiteParams{..} = do
    path <- case sdpLocation of
        SQLiteInMemory ->
            return ":memory:"
        SQLiteReal path ->
            -- some paths produce db in memory, can't use them
            if any (== path) ["", ":memory:"]
            then throwM (InvalidSQLitePathException path)
            else return path

    sdConn <- liftIO $ Lower.open path
    return SQLiteDB {..}

closeSQLiteDB :: MonadIO m => SQLiteDB -> m ()
closeSQLiteDB = liftIO . Lower.close . sdConn

------------------------------------------------------------
-- Instances
------------------------------------------------------------

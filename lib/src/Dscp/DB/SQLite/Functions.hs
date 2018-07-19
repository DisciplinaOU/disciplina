{-# LANGUAGE TypeApplications #-}

module Dscp.DB.SQLite.Functions
       ( -- * Closing/opening
         openSQLiteDB
       , closeSQLiteDB
       ) where

import qualified Database.SQLite.Simple as Backend

import UnliftIO (UnliftIO(..), askUnliftIO)

import Loot.Base.HasLens (HasLens (..))

import Dscp.DB.SQLite.Class (MonadSQLiteDB (..))
import Dscp.DB.SQLite.Error (SQLConnectionOpenningError (..), rethrowSQLRequestError)
import Dscp.DB.SQLite.Types (SQLiteDB (..), SQLiteDBLocation (..), SQLiteParams (..))
import Dscp.Launcher.Rio (RIO, runRIO)
import Dscp.Util (wrapRethrowIO)

-----------------------------------------------------------
-- Opening/closing
-----------------------------------------------------------

openSQLiteDB
    :: (MonadIO m, MonadCatch m)
    => SQLiteParams -> m SQLiteDB
openSQLiteDB SQLiteParams{..} = do
    path <- case sdpLocation of
        SQLiteInMemory ->
            return ":memory:"
        SQLiteReal path
            -- some paths produce db in memory, can't use them
            | any (== path) ["", ":memory:"] ->
                throwM (SQLInvalidPathError path)
            | otherwise ->
                return path
    sdConn <-
        wrapRethrowIO @SomeException (SQLConnectionOpenningError . show) $
        Backend.open path
    return SQLiteDB {..}

closeSQLiteDB :: MonadIO m => SQLiteDB -> m ()
closeSQLiteDB = liftIO . Backend.close . sdConn

------------------------------------------------------------
-- Instances
------------------------------------------------------------

instance HasLens SQLiteDB ctx SQLiteDB => MonadSQLiteDB (RIO ctx) where
    query q params =
        rethrowSQLRequestError $ do
            SQLiteDB{..} <- view $ lensOf @SQLiteDB
            liftIO $ Backend.query sdConn q params
    queryStreamed q params acc f =
        rethrowSQLRequestError $ do
            ctx <- ask
            let SQLiteDB{..} = ctx ^. lensOf @SQLiteDB
            liftIO $ Backend.fold sdConn q params acc (runRIO ctx ... f)
    execute q params = do
        rethrowSQLRequestError $ do
            SQLiteDB{..} <- view $ lensOf @SQLiteDB
            liftIO $ Backend.execute sdConn q params

    transaction action = do
        UnliftIO unlift <- askUnliftIO
        SQLiteDB conn <- view $ lensOf @SQLiteDB
        liftIO $ do
            conn `Backend.withTransaction` do
                unlift action

    traced action = do
        UnliftIO unlift <- askUnliftIO
        SQLiteDB conn <- view $ lensOf @SQLiteDB
        liftIO $ do
            Backend.setTrace conn (Just print)
            res <- unlift action
            Backend.setTrace conn Nothing
            return res
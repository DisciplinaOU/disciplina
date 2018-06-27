{-# LANGUAGE TypeApplications #-}

module Dscp.DB.SQLite.Functions
       ( -- * Closing/opening
         openSQLiteDB
       , closeSQLiteDB

         -- * For database initialisation
       , ConnectionReader (..)
       ) where

import qualified Database.SQLite.Simple as Lower
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
        SQLiteReal path ->
            -- some paths produce db in memory, can't use them
            if any (== path) ["", ":memory:"]
            then throwM (SQLInvalidPathError path)
            else return path

    sdConn <-
        wrapRethrowIO @SomeException (SQLConnectionOpenningError . show) $
        Lower.open path
    return SQLiteDB {..}

closeSQLiteDB :: MonadIO m => SQLiteDB -> m ()
closeSQLiteDB = liftIO . Lower.close . sdConn

------------------------------------------------------------
-- Instances
------------------------------------------------------------

instance HasLens SQLiteDB ctx SQLiteDB => MonadSQLiteDB (RIO ctx) where
    query q params =
        rethrowSQLRequestError $ do
            SQLiteDB{..} <- view $ lensOf @SQLiteDB
            liftIO $ Lower.query sdConn q params
    queryStreamed q params acc f =
        rethrowSQLRequestError $ do
            ctx <- ask
            let SQLiteDB{..} = ctx ^. lensOf @SQLiteDB
            liftIO $ Lower.fold sdConn q params acc (runRIO ctx ... f)
    execute q params = do
        rethrowSQLRequestError $ do
            SQLiteDB{..} <- view $ lensOf @SQLiteDB
            liftIO $ Lower.execute sdConn q params

-- Just 'ReaderT' overlaps with RIO instances.
newtype ConnectionReader a = ConnectionReader
    { getConnectionReader :: ReaderT SQLiteDB IO a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadSQLiteDB ConnectionReader where
    query q params = ConnectionReader $ ReaderT $ \(SQLiteDB conn) -> do
        liftIO $ Lower.query conn q params

    queryStreamed q params acc f = ConnectionReader $ ReaderT $ \(SQLiteDB conn) ->
        liftIO $ Lower.fold conn q params acc $ \acc row ->
            getConnectionReader (f acc row) `runReaderT` SQLiteDB conn

    execute q params = ConnectionReader $ ReaderT $ \(SQLiteDB conn) ->
        liftIO $ Lower.execute conn q params
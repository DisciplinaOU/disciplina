{-# LANGUAGE TypeApplications #-}

module Dscp.DB.SQLite.Functions
       ( -- * Closing/opening
         openSQLiteDB
       , closeSQLiteDB

         -- * Operations with connections
       , borrowConnection
       , forEachConnection

         -- * SQLite context
       , DBT
       , WithinTx
       , query
       , execute
       , traced
       , invoke
       , transact
       , invokeUnsafe
       ) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import qualified Data.List as L
import Database.SQLite.Simple (Connection, FromRow, Query, ToRow)
import qualified Database.SQLite.Simple as Backend
import qualified Loot.Log as Log
import Time (Millisecond, sec, toNum, toUnit)
import UnliftIO (MonadUnliftIO (..), UnliftIO (..))
import qualified UnliftIO as UIO

import Loot.Base.HasLens (HasCtx, HasLens (..))
import UnliftIO (askUnliftIO)

import Dscp.DB.SQLite.Error
import Dscp.DB.SQLite.Types
import Dscp.Util (wrapRethrowIO)

-----------------------------------------------------------
-- Operations with plain connections
-----------------------------------------------------------

-- | Increase counter of pending threads, if it exceedes maximum - throw.
notingPending
    :: (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB])
    => m a -> m a
notingPending action = do
    db <- view (lensOf @SQLiteDB)
    let pendingNum = sdPendingNum db
        maxPending = sdMaxPending db
    UIO.bracket_ (increaseCounterOrThrow pendingNum maxPending)
                 (decreaseCounter pendingNum)
                 action
  where
    increaseCounterOrThrow pendingNum maxPending =
        atomically $ do
            pending <- readTVar pendingNum
            when (pending >= maxPending) $
                throwM SQLRequestsNumberExceeded
            writeTVar pendingNum $! pending + 1
    decreaseCounter pendingNum =
        atomically $ modifyTVar' pendingNum pred

-- | Temporaly take a connection, exclusively.
borrowConnection
    :: (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB])
    => (Connection -> m a) -> m a
borrowConnection action = do
    -- TODO: timeout?
    -- TODO: drop warnings on long execution?
    db <- view (lensOf @SQLiteDB)
    let connPool = sdConnPool db
    UIO.bracket (notingPending $ liftIO $ readChan connPool)
                (liftIO . writeChan connPool)
                action

-- | Execute a given action for every connection in pool.
-- Pool will be emptied for a while.
forEachConnection
    :: MonadIO m => SQLiteDB -> (Connection -> m ()) -> m ()
forEachConnection sd action = do
    conns <- replicateM (sdConnNum sd) $ do
        conn <- liftIO $ readChan (sdConnPool sd)
        action conn
        return conn
    liftIO . forM_ conns $ writeChan (sdConnPool sd)

-----------------------------------------------------------
-- Opening/closing
-----------------------------------------------------------

openSQLiteDB
    :: (MonadIO m, MonadCatch m)
    => SQLiteParams -> m SQLiteDB
openSQLiteDB params = do
    (path, connNum, maxPending) <- case sdpMode params of
        SQLiteInMemory ->
            return (":memory:", 1, 99999)
        SQLiteReal realParams -> do
            let path = srpPath realParams
                mConnNum = srpConnNum realParams
                maxPending = srpMaxPending realParams
            -- some paths produce db in memory, can't use them
            when (any (== path) ["", ":memory:"]) $
                throwM (SQLInvalidPathError path)

            connNum <- case mConnNum of
                Nothing  -> liftIO $ max 1 . pred <$> getNumCapabilities
                Just num -> pure num

            return (path, connNum, maxPending)

    unless (connNum > 0) $
        throwM $ SQLInvalidConnectionsNumber connNum
    unless (maxPending >= 0) $
        throwM $ SQLInvalidMaxPendingNumber connNum

    connPool <- liftIO newChan
    pendingThreadsNum <- newTVarIO 0

    wrapRethrowIO @SomeException (SQLConnectionOpenningError . show) $ do
        conns <- replicateM connNum $ do
            conn <- Backend.open path
            writeChan connPool conn
            setBusyTimeout conn (toUnit @Millisecond $ sec 60)
            return conn
        setWALMode (L.head conns)

    return SQLiteDB
        { sdConnNum = connNum
        , sdConnPool = connPool
        , sdPendingNum = pendingThreadsNum
        , sdMaxPending = maxPending
        }
  where
    setBusyTimeout conn timeout =
        let timeoutT = show (toNum @Millisecond @Int timeout)
        in Backend.execute conn
            (fromString $ "pragma busy_timeout = " <> timeoutT)
            ()
    setWALMode conn =
        Backend.execute conn "PRAGMA journal_mode = WAL" ()

closeSQLiteDB :: MonadIO m => SQLiteDB -> m ()
closeSQLiteDB sd =
    -- we return closed connections back to pool, because pending requests to DB
    -- would better throw an exception trying to operate with closed connection
    -- rather than just hang.
    liftIO $ forEachConnection sd Backend.close

------------------------------------------------------------
-- SQLite context
------------------------------------------------------------

-- | Single pack of DB operations.
-- Phantom type parameter @ r @ should be either type variable or 'WithinTx'
-- and means whether actions should happen within transaction.
newtype DBT r m a = DBT
    { runDBT :: ReaderT Connection m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadUnliftIO m => MonadUnliftIO (DBT r m) where
    askUnliftIO = do
        UnliftIO unlift <- DBT askUnliftIO
        return $ UnliftIO $ unlift . runDBT

instance (Log.MonadLogging m, Monad m) => Log.MonadLogging (DBT r m) where
    log = DBT . lift ... Log.log
    logName = DBT $ lift Log.logName

-- | Use this in 'DBT' to mark that actions should be performed within
-- transaction.
data WithinTx

-- | Make an SQL query which returns some result.
query
    :: (MonadIO m, FromRow row, ToRow params)
    => Query -> params -> DBT r m [row]
query q params = do
    conn <- DBT ask
    liftIO $ Backend.query conn q params

-- | Perform an SQL query which does not return any result.
execute
    :: (MonadIO m, ToRow params)
    => Query -> params -> DBT r m ()
execute q params = do
    conn <- DBT ask
    liftIO $ Backend.execute conn q params

-- | Enables SQLite tracing locally. For debug purposes.
--
-- Note: if any trace handler was set globally, it will be lost after that.
traced :: MonadUnliftIO m => DBT r m a -> DBT r m a
traced action = do
    conn <- DBT ask
    UIO.bracket_
        (liftIO $ Backend.setTrace conn (Just print))
        (liftIO $ Backend.setTrace conn Nothing)
        action

-- | Run 'DBT' without carying about whether it assumes to be run in transaction
-- or not.
invokeUnsafe
    :: (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB])
    => DBT r m a -> m a
invokeUnsafe (DBT action) =
    borrowConnection $ runReaderT action

-- | Internal type, indicates that actions happen not in 'transact'.
data OutsideOfTransaction

-- | Run 'DBT'.
invoke
    :: (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB])
    => DBT OutsideOfTransaction m a -> m a
invoke (DBT action) =
    borrowConnection $ runReaderT action

-- | Run 'DBT' within a transaction.
-- This function is polymorphic over @r@ on purpose, this way it cannot be
-- applied to @forall r m a. DBT r m a@. If you encounter an error due to this,
-- you are probably doing something wrong (@martoon).
transact
    :: forall r m ctx a.
       (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB])
    => RequiresTransaction r => DBT r m a -> m a
transact (DBT action) = do
    UnliftIO unlift <- askUnliftIO
    borrowConnection $ \conn ->
        liftIO . Backend.withImmediateTransaction conn $
            unlift $ runReaderT action conn

-- | Helps to prevent using 'transact' when 'invoke' is enough.
class RequiresTransaction r
instance RequiresTransaction WithinTx

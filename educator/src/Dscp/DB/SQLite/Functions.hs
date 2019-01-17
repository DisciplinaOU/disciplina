{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Dscp.DB.SQLite.Functions
       ( -- * Closing/opening
         openSQLiteDB
       , closeSQLiteDB

         -- * Operations with connections
       , borrowConnection
       , forEachConnection

         -- * SQLite context
       , DBT
       , TransactionalContext (WithinTx)
       , OperationType (Writing)

         -- * SQLite queries building
       , runSelect
       , runSelectMap
       , runInsert
       , runInsertReturning
       , runUpdate
       , runDelete
       , withConnection

         -- * Runners
       , invoke
       , invokeUnsafe
       , transactR
       , transactW

         -- * Misc
       , traced

         -- * Internal
       , sqliteMToDbt
       , dbtToSqliteM
       ) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad.Reader (mapReaderT)
import qualified Data.List as L
import Database.Beam.Backend (FromBackendRow, MonadBeam (..))
import qualified Database.Beam.Backend.SQL.BeamExtensions as Backend
import Database.Beam.Backend.SQL.SQL92 (Sql92DeleteSyntax, Sql92InsertSyntax, Sql92SelectSyntax,
                                        Sql92UpdateSyntax)
import Database.Beam.Query (QExpr, SqlDelete, SqlInsert, SqlInsertValues, SqlSelect, SqlUpdate)
import qualified Database.Beam.Query as Backend
import Database.Beam.Schema (DatabaseEntity, TableEntity)
import Database.Beam.Sqlite (Sqlite, SqliteCommandSyntax, SqliteM (..))
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as Backend
import Loot.Base.HasLens (HasCtx, HasLens (..))
import qualified Loot.Log as Log
import qualified System.Console.ANSI as ANSI
import Time (Millisecond, sec, toNum, toUnit)
import UnliftIO (MonadUnliftIO (..), UnliftIO (..), askUnliftIO)
import qualified UnliftIO as UIO

import Dscp.Config
import Dscp.DB.SQLite.Error
import Dscp.DB.SQLite.Types
import Dscp.Util

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

-- | Execute a given action for every connection in pool, in parallel with no any other
-- action.
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
    => SQLiteParamsRec -> m SQLiteDB
openSQLiteDB params = do
    (path, connNum, maxPending) <- case params ^. tree #mode . selection of
        "inMemory" -> return (":memory:", 1, 99999)
        "real" -> do
            let realParams = params ^. tree #mode . peekBranch #real
                path       = realParams ^. option #path
                mConnNum   = realParams ^. option #connNum
                maxPending = realParams ^. option #maxPending
            -- some paths produce db in memory, can't use them
            when (path `elem` ["", ":memory:"]) $
                throwM (SQLInvalidPathError path)

            connNum <- case mConnNum of
                Nothing  -> liftIO $ max 1 . pred <$> getNumCapabilities
                Just num -> pure num

            return (path, connNum, maxPending)
        sel -> error $ "unknown SQLiteDBMode type: " <> fromString sel

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
-- Phantom type parameter @ t @ should be either a type variable or 'WithinTx'
-- and means whether actions should happen within transaction.
-- Phantom type parameter @ w @ should be either a type variable or 'Writing'
-- and means whether given actions should be performed in writing transaction,
-- if performed within a transaction at all.
--
-- Notice that for the sake of isolation we do not provide 'MonadTrans' instance,
-- though fetching info from context of the inner monad is allowed.
newtype DBT (t :: TransactionalContext) (w :: OperationType) m a = DBT
    { runDBT :: ReaderT Connection m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadUnliftIO m => MonadUnliftIO (DBT t w m) where
    askUnliftIO = do
        UnliftIO unlift <- DBT askUnliftIO
        return $ UnliftIO $ unlift . runDBT

instance MonadReader r m => MonadReader r (DBT t w m) where
    ask = DBT $ lift ask
    reader = DBT . lift . reader
    local doModify = DBT . mapReaderT (local doModify) . runDBT

instance (Log.MonadLogging m, Monad m) => Log.MonadLogging (DBT t w m) where
    log = DBT . lift ... Log.log
    logName = DBT $ lift Log.logName

instance (Log.ModifyLogName m, Monad m) => Log.ModifyLogName (DBT t w m) where
    modifyLogNameSel how = DBT . mapReaderT (Log.modifyLogNameSel how) . runDBT

dbtToSqliteM :: MonadUnliftIO m => DBT t w m (DBT t w m a -> SqliteM a)
dbtToSqliteM = do
    UnliftIO unliftIO <- DBT $ lift askUnliftIO
    return $ \(DBT action) ->
        SqliteM . ReaderT $ \(_logger, conn) -> unliftIO (runReaderT action conn)

sqliteMToDbt :: MonadIO m => SqliteM a -> DBT t w m a
sqliteMToDbt (SqliteM action) =
    DBT . ReaderT $ \conn -> liftIO $ runReaderT action (logger, conn)
  where
    logger _ = pass
    -- logger = sqlDebugLogger

instance MonadUnliftIO m =>
         MonadBeam SqliteCommandSyntax Sqlite Connection (DBT t w m) where
    -- We omit implementation of the following methods as soon as they work in IO only,
    -- use dedicated 'DBT' runners instead ('runInsert' and others).
    withDatabaseDebug = error "DBT.withDatabaseDebug: not implemented"
    withDatabase = error "DBT.withDatabase: not implemented"
    runNoReturn = error "DBT.runNoReturn: not implemented"
    runReturningMany = error "DBT.runReturningMany: not implemented"

-- | Declares whether given 'DBT' actions should be performed within
-- transaction.
data TransactionalContext = WithinTx | OutsideOfTransaction

-- | Declares whether given 'DBT' actions, if performed within transaction,
-- should be performed within /write/ transaction.
-- We cannot rely on sqlite itself detecting writing transactions, otherwise
-- "ErrorBusy: database is locked" errors are possible,
-- see https://stackoverflow.com/questions/30438595/sqlite3-ignores-sqlite3-busy-timeout
-- for details.
data OperationType = Writing | Reading

------------------------------------------------------------
-- SQLite queries building
------------------------------------------------------------

{- We rewrite runners as soon as it allows requiring write or transaction context.
-}

-- | Run select query.
-- We abandon different 'runSelectReturningOne' and 'runSelectReturningList' versions
-- for the sake of better unexpected behavior processing.
runSelect
    :: (MonadIO m, FromBackendRow Sqlite a)
    => SqlSelect (Sql92SelectSyntax SqliteCommandSyntax) a -> DBT t w m [a]
runSelect = sqliteMToDbt . Backend.runSelectReturningList

-- | Run select query and modify fetched results.
runSelectMap
    :: (MonadIO m, FromBackendRow Sqlite a)
    => (a -> b) -> SqlSelect (Sql92SelectSyntax SqliteCommandSyntax) a -> DBT t w m [b]
runSelectMap f = fmap (map f) . runSelect

runInsert
    :: MonadIO m
    => SqlInsert (Sql92InsertSyntax SqliteCommandSyntax) -> DBT t 'Writing m ()
runInsert = sqliteMToDbt . Backend.runInsert

runInsertReturning
    :: (MonadIO m, _)
    => DatabaseEntity be db (TableEntity table)
    -> SqlInsertValues _ (table (QExpr _ _))
    -> DBT t 'Writing m [table Identity]
runInsertReturning db = sqliteMToDbt . Backend.runInsertReturningList db

runUpdate
    :: MonadIO m
    => SqlUpdate (Sql92UpdateSyntax SqliteCommandSyntax) tbl -> DBT t 'Writing m ()
runUpdate = sqliteMToDbt . Backend.runUpdate

runDelete
    :: MonadIO m
    => SqlDelete (Sql92DeleteSyntax SqliteCommandSyntax) tbl -> DBT t 'Writing m ()
runDelete = sqliteMToDbt . Backend.runDelete

-- | For low-level operations.
withConnection :: MonadIO m => (Connection -> IO a) -> DBT t w m a
withConnection f = DBT $ ReaderT (liftIO . f)

------------------------------------------------------------
-- DBT runners
------------------------------------------------------------

-- | Run 'DBT' without carying about whether it assumes to be run in transaction
-- or not.
invokeUnsafe
    :: (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB])
    => DBT t w m a -> m a
invokeUnsafe (DBT action) =
    borrowConnection $ runReaderT action

-- | Run 'DBT'.
invoke
    :: (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB])
    => DBT 'OutsideOfTransaction w m a -> m a
invoke (DBT action) =
    borrowConnection $ runReaderT action

-- | Run 'DBT' within a transaction.
transactUsing
    :: (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB])
    => (forall x. Connection -> IO x -> IO x) -> DBT t w m a -> m a
transactUsing withTransaction (DBT action) = do
    UnliftIO unlift <- askUnliftIO
    borrowConnection $ \conn ->
        liftIO . withTransaction conn $
            unlift $ runReaderT action conn

-- | Run 'DBT' within a transaction.
-- This function is polymorphic over @r@ on purpose, this way it cannot be
-- applied to @forall r m a. DBT t m a@. If you encounter an error due to this,
-- you are probably doing something wrong (@martoon).
transactR
    :: forall t m ctx a.
       (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB])
    => RequiresTransaction t => DBT t 'Reading m a -> m a
transactR = transactUsing Backend.withTransaction

-- | Run 'DBT' within a writing transaction.
transactW
    :: forall t w m ctx a.
       (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB])
    => (RequiresTransaction t, RequiresWriting w) => DBT t w m a -> m a
transactW = transactUsing Backend.withImmediateTransaction

-- | Helps to prevent using 'transact' when 'invoke' is enough.
class RequiresTransaction (t :: TransactionalContext)
instance RequiresTransaction 'WithinTx

-- | Helps to prevent using 'transactW' when 'transact' is enough.
class RequiresWriting (w :: OperationType)
instance RequiresWriting 'Writing

------------------------------------------------------------
-- Misc
------------------------------------------------------------

-- | Used for debug logging.
sqlDebugLogger :: String -> IO ()
sqlDebugLogger msg =
    putStrLn $ mconcat
    [ ANSI.setSGRCode [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Magenta]
    , "SQL>"
    , ANSI.setSGRCode [ANSI.Reset]
    , " "
    , msg
    ]

-- | Enables SQLite tracing locally. For debug purposes.
--
-- Note: if any trace handler was set globally, it will be lost after that.
traced :: MonadUnliftIO m => DBT t w m a -> DBT t w m a
traced action = do
    conn <- DBT ask
    UIO.bracket_
        (liftIO $ Backend.setTrace conn (Just $ sqlDebugLogger . toString))
        (liftIO $ Backend.setTrace conn Nothing)
        action

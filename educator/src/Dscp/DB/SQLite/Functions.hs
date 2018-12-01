{-# LANGUAGE GADTs                 #-}
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

         -- * Backends
       , SQLBackend (..)
       , SomeSQLBackend (..)

         -- * SQLite context
       , WithinTx
       , WithinWrite
       , WithinWriteTx

         -- * SQLite queries building
       , MonadQuery
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
       , invoke_
       , transactW_
       , transactR_

         -- * Misc
       , traced
       ) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import qualified Data.List as L
import Data.Reflection (Given (..), give)
import Database.Beam.Backend (FromBackendRow, MonadBeam (..))
import qualified Database.Beam.Backend.SQL.BeamExtensions as Backend
import Database.Beam.Backend.SQL.SQL92 (IsSql92Syntax, Sql92DeleteSyntax, Sql92InsertSyntax,
                                        Sql92SelectSyntax, Sql92UpdateSyntax)
import Database.Beam.Query (QExpr, SqlDelete, SqlInsert, SqlInsertValues, SqlSelect, SqlUpdate)
import qualified Database.Beam.Query as Query
import Database.Beam.Schema (Beamable, DatabaseEntity, TableEntity)
import Database.Beam.Sqlite (Sqlite, SqliteCommandSyntax, SqliteM (..))
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as Backend
import Loot.Base.HasLens (HasCtx, HasLens (..))
import qualified System.Console.ANSI as ANSI
import Time (Millisecond, sec, toNum, toUnit)
import UnliftIO (MonadUnliftIO (..))
import qualified UnliftIO as UIO

import Dscp.DB.SQLite.Error
import Dscp.DB.SQLite.Instances
import Dscp.DB.SQLite.Types
import Dscp.Util

-- TODO remove dependency on instances ^

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
            when (path `elem` ["", ":memory:"]) $
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

data TransactionContextProvided = TransactionContextProvided
type WithinTx = Given TransactionContextProvided

data WriteContextProvided = WriteContextProvided
type WithinWrite = Given WriteContextProvided

type WithinWriteTx = (WithinTx, WithinWrite)

allowTxUnsafe :: (WithinTx => a) -> a
allowTxUnsafe = give TransactionContextProvided

allowWriteUnsafe :: (WithinWrite => a) -> a
allowWriteUnsafe a = give WriteContextProvided $ allowTxUnsafe a

------------------------------------------------------------
-- SQLite queries building
------------------------------------------------------------

{- We rewrite runners as soon as it allows requiring write context.
-}

-- | Run select query.
-- We abandon different 'runSelectReturningOne' and 'runSelectReturningList' versions
-- for the sake of better unexpected behavior processing.
runSelect
    :: forall m cmd be hdl a.
       (MonadBeam cmd be hdl m, IsSql92Syntax cmd, FromBackendRow be a)
    => SqlSelect (Sql92SelectSyntax cmd) a -> m [a]
runSelect = Query.runSelectReturningList

-- | Run select query and modify fetched results.
runSelectMap
    :: (MonadBeam cmd be hdl m, IsSql92Syntax cmd, FromBackendRow be a)
    => (a -> b) -> SqlSelect (Sql92SelectSyntax cmd) a -> m [b]
runSelectMap f = fmap (map f) . runSelect

runInsert
    :: (MonadBeam cmd be hdl m, IsSql92Syntax cmd, WithinWrite)
    => SqlInsert (Sql92InsertSyntax cmd) -> m ()
runInsert = Query.runInsert

runInsertReturning
    :: (Beamable table, WithinWrite,
        FromBackendRow be (table Identity),
        Backend.MonadBeamInsertReturning cmd be hdl m)
    => DatabaseEntity be db (TableEntity table)
    -> SqlInsertValues _ (table (QExpr _ _))
    -> m [table Identity]
runInsertReturning db = Backend.runInsertReturningList db

runUpdate
    :: (MonadBeam cmd be hdl m, IsSql92Syntax cmd, WithinWrite)
    => SqlUpdate (Sql92UpdateSyntax cmd) tbl -> m ()
runUpdate = Query.runUpdate

runDelete
    :: (MonadBeam cmd be hdl m, IsSql92Syntax cmd, WithinWrite)
    => SqlDelete (Sql92DeleteSyntax cmd) tbl -> m ()
runDelete = Query.runDelete

-- | For low-level operations.
withConnection :: (Connection -> IO a) -> SqliteM a
withConnection f = SqliteM . ReaderT $ \(_, conn) -> f conn

------------------------------------------------------------
-- DB endpoints runners
------------------------------------------------------------

-- | Tagged specifier of SQL backend engine.
data SQLBackend cmd be hdl bm where
    SQLiteBackend :: SQLBackend SqliteCommandSyntax Sqlite Connection SqliteM

-- | Untagged specifier of SQL backend engine.
data SomeSQLBackend where
    SomeSQLBackend
        :: MonadQueryFull cmd be hdl bm
        => SQLBackend cmd be hdl bm -> SomeSQLBackend

-- | Helper to set backend monad.
restrictBackendMonad
    :: (Monad bm, MonadQuery cmd be Connection bm)
    => SQLBackend cmd be hdl bm -> bm ()
restrictBackendMonad _ = pass

-- | A query generalized over an SQL engine.
type Query extraCtx a =
    forall cmd be hdl bm.
       (MonadQueryFull cmd be hdl bm, extraCtx)
    => bm a

-- | Run DB action with all the missing constraints supplied as an argument.
invoke
    :: forall a ctx m.
       (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB, SomeSQLBackend])
    => Query WithinWrite a -> m a
invoke = invokeUnsafe

-- | Run DB action without carying about whether it assumes to be run in transaction.
-- or not. All the missing constraints supplied as an argument
invokeUnsafe
    :: forall a ctx m.
       (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB, SomeSQLBackend])
    => Query WithinWriteTx a -> m a
invokeUnsafe action = do
    SomeSQLBackend sqlBackend@SQLiteBackend{} <- view $ lensOf @SomeSQLBackend
    let _ = restrictBackendMonad sqlBackend
    borrowConnection $ \conn ->
        liftIO $ withDatabase conn $ do
            restrictBackendMonad sqlBackend
            allowTxUnsafe $ allowWriteUnsafe action

-- | Run DB action within a transaction.
transactUsing
    :: (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB, SomeSQLBackend])
    => (forall x. Connection -> IO x -> IO x)
    -> Query () a
    -> m a
transactUsing withTransaction action = do
    SomeSQLBackend sqlBackend@SQLiteBackend{} <- view $ lensOf @SomeSQLBackend
    borrowConnection $ \conn ->
        liftIO . withTransaction conn $
            withDatabase conn (restrictBackendMonad sqlBackend >> action)

-- | Run DB action within a transaction with all the missing constraints supplied as an
-- argument.
transactR
    :: (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB, SomeSQLBackend])
    => Query WithinTx a
    -> m a
transactR action =
    allowTxUnsafe $ transactUsing Backend.withTransaction action

-- | Run DB action within a writing transaction with all the missing constraints supplied
-- as an argument.
transactW
    :: (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB, SomeSQLBackend])
    => Query WithinWriteTx a
    -> m a
transactW action =
    allowWriteUnsafe $ allowTxUnsafe $
        transactUsing Backend.withImmediateTransaction action

-- | Run DB action assuming that nothing is returned.
invoke_
    :: (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB, SomeSQLBackend])
    => Query WithinWrite ()
    -> m ()
invoke_ action = invoke action

-- | Run DB action within a transaction assuming that nothing is returned.
transactR_
    :: (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB, SomeSQLBackend])
    => Query WithinTx a
    -> m a
transactR_ action = transactW action

-- | Run DB action within a writing transaction assuming that nothing is returned.
transactW_
    :: (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB, SomeSQLBackend])
    => Query WithinWriteTx ()
    -> m ()
transactW_ action = transactW action

------------------------------------------------------------
-- Orphans
------------------------------------------------------------

deriving instance MonadThrow SqliteM
deriving instance MonadCatch SqliteM

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

-- | Enables SQL queries tracing locally. For debug purposes.
traced :: MonadUnliftIO m => SqliteM a -> SqliteM a
traced = SqliteM . local (_1 .~ sqlDebugLogger) . runSqliteM

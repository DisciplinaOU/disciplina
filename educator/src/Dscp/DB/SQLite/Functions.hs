{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Dscp.DB.SQLite.Functions
       ( -- * Closing/opening
         openPostgresDB
       , closePostgresDB

         -- * Operations with connections
       , borrowConnection
       , forEachConnection

         -- * SQLite context
       , MonadQuery
       , WithinTx

         -- * SQLite queries building
       , runSelect
       , runSelectMap
       , Query.runInsert
       , runInsertReturning
       , Query.runUpdate
       , Query.runDelete

         -- * Runners
       , invoke
       , invokeUnsafe
       , transact

         -- * Misc
       , traced

         -- * Unsafe
       , allowTxUnsafe
       ) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Lens (views)
import qualified Control.Monad.Catch as Catch
import Data.Reflection (Given, give)
import Database.Beam.Backend (FromBackendRow, MonadBeam (..))
import qualified Database.Beam.Backend.SQL.BeamExtensions as Backend
import Database.Beam.Backend.SQL.SQL92 (IsSql92Syntax, Sql92SelectSyntax)
import Database.Beam.Postgres (Pg)
import Database.Beam.Query (QExpr, SqlInsertValues, SqlSelect)
import qualified Database.Beam.Query as Query
import Database.Beam.Schema (Beamable, DatabaseEntity, TableEntity)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as Backend
import qualified Database.PostgreSQL.Simple.Transaction as Backend
import Loot.Base.HasLens (HasCtx, HasLens (..))
import qualified System.Console.ANSI as ANSI
import UnliftIO (MonadUnliftIO (..))
import qualified UnliftIO as UIO

import Dscp.DB.SQLite.Error
import Dscp.DB.SQLite.Types
import Dscp.Util

-----------------------------------------------------------
-- Operations with plain connections
-----------------------------------------------------------

-- | Increase counter of pending threads, if it exceedes maximum - throw.
notingPending
    :: (MonadUnliftIO m, HasCtx ctx m '[SQL])
    => m a -> m a
notingPending action = do
    db <- view (lensOf @SQL)
    let pendingNum = sqlPendingNum db
        maxPending = sqlMaxPending db
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
    :: (MonadUnliftIO m, HasCtx ctx m '[SQL])
    => (Connection -> m a) -> m a
borrowConnection action = do
    -- TODO: timeout?
    -- TODO: drop warnings on long execution?
    db <- view (lensOf @SQL)
    let connPool = sqlConnPool db
    UIO.bracket (notingPending $ liftIO $ readChan connPool)
                (liftIO . writeChan connPool)
                action

-- | Execute a given action for every connection in pool, in parallel with no any other
-- action.
-- Pool will be emptied for a while.
forEachConnection
    :: MonadIO m => SQL -> (Connection -> m ()) -> m ()
forEachConnection sd action = do
    conns <- replicateM (sqlConnNum sd) $ do
        conn <- liftIO $ readChan (sqlConnPool sd)
        action conn
        return conn
    liftIO . forM_ conns $ writeChan (sqlConnPool sd)

-----------------------------------------------------------
-- Opening/closing
-----------------------------------------------------------

openPostgresDB
    :: (MonadIO m, MonadCatch m)
    => PostgresParams -> m SQL
openPostgresDB params = do
    (ConnectionString connStr, connNum, maxPending, txSwitch) <- case ppMode params of
        PostgresReal PostgresRealParams
            { prpConnString = connStr, prpConnNum = mConnNum
            , prpMaxPending = maxPending
            } -> do
                connNum <- case mConnNum of
                    Nothing  -> liftIO $ max 1 . pred <$> getNumCapabilities
                    Just num -> pure num
                return (connStr, connNum, maxPending, TransactionsOn)
        PostgresTest PostgresTestParams
            { ptpConnString = connStr
            } -> do
                return (connStr, 1, 1000000000, TransactionsOff)

    unless (connNum > 0) $
        throwM $ SQLInvalidConnectionsNumber connNum
    unless (maxPending >= 0) $
        throwM $ SQLInvalidMaxPendingNumber connNum

    connPool <- liftIO newChan
    pendingThreadsNum <- newTVarIO 0

    wrapRethrowIO @SomeException (SQLConnectionOpenningError . show) $ do
        replicateM_ connNum $
            Backend.connectPostgreSQL connStr >>= writeChan connPool

    return SQL
        { sqlConnNum = connNum
        , sqlConnPool = connPool
        , sqlPendingNum = pendingThreadsNum
        , sqlMaxPending = maxPending
        , sqlTransactionsSwitch = txSwitch
        }

closePostgresDB :: MonadIO m => SQL -> m ()
closePostgresDB sd =
    -- we return closed connections back to pool, because pending requests to DB
    -- would better throw an exception trying to operate with closed connection
    -- rather than just hang.
    liftIO $ forEachConnection sd Backend.close

------------------------------------------------------------
-- SQLite context
------------------------------------------------------------

data TransactionContextProvided = TransactionContextProvided
type WithinTx = Given TransactionContextProvided

allowTxUnsafe :: (WithinTx => a) -> a
allowTxUnsafe = give TransactionContextProvided

------------------------------------------------------------
-- SQL queries building
------------------------------------------------------------

type MonadQuery m = (m ~ Pg)

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

runInsertReturning
    :: (Beamable table,
        FromBackendRow be (table Identity),
        Backend.MonadBeamInsertReturning cmd be hdl m)
    => DatabaseEntity be db (TableEntity table)
    -> SqlInsertValues _ (table (QExpr _ _))
    -> m [table Identity]
runInsertReturning db = Backend.runInsertReturningList db

------------------------------------------------------------
-- DB endpoints runners
------------------------------------------------------------

type Query extraCtx a = extraCtx => Pg a

-- | Run DB action with all the missing constraints supplied as an argument.
invoke
    :: forall a ctx m.
       (MonadUnliftIO m, HasCtx ctx m '[SQL])
    => Query () a -> m a
invoke = invokeUnsafe

-- | Run DB action without carying about whether it assumes to be run in transaction.
-- or not. All the missing constraints supplied as an argument
invokeUnsafe
    :: forall a ctx m.
       (MonadUnliftIO m, HasCtx ctx m '[SQL])
    => Query WithinTx a -> m a
invokeUnsafe action =
    borrowConnection $ \conn ->
        liftIO $ withDatabase conn $ do
            allowTxUnsafe action

-- | Run DB action within a transaction with all the missing constraints supplied as an
-- argument.
transact
    :: (MonadUnliftIO m, HasCtx ctx m '[SQL])
    => Query WithinTx a
    -> m a
transact action = do
    mode <- views (lensOf @SQL) sqlTransactionsSwitch
    borrowConnection $ \conn ->
        liftIO . inTransaction mode conn $
            withDatabase conn $ allowTxUnsafe action
  where
    inTransaction = \case
        TransactionsOn -> Backend.withTransactionSerializable
        TransactionsOff -> \_conn -> id

------------------------------------------------------------
-- Orphans
------------------------------------------------------------

instance MonadThrow Pg where
    throwM = liftIO . Catch.throwM
instance MonadCatch Pg where
    catch = const

------------------------------------------------------------
-- Misc
------------------------------------------------------------

-- | Used for debug logging.
_sqlDebugLogger :: String -> IO ()
_sqlDebugLogger msg =
    putStrLn $ mconcat
    [ ANSI.setSGRCode [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Magenta]
    , "SQL>"
    , ANSI.setSGRCode [ANSI.Reset]
    , " "
    , msg
    ]

-- | Enables SQL queries tracing locally. For debug purposes.
traced :: MonadUnliftIO m => Pg a -> Pg a
traced =  error ":("  -- Pg . local (\(_logger, conn) -> (sqlDebugLogger, conn)) . runSqliteM

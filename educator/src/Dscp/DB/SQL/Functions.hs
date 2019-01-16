{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Dscp.DB.SQL.Functions
       ( -- * Closing/opening
         openPostgresDB
       , closePostgresDB

         -- * Operations with connections
       , borrowConnection
       , forEachConnection

         -- * SQL context
       , DBT
       , TransactionalContext (WithinTx)

         -- * SQL queries building
       , runSelect
       , runSelectMap
       , runInsert
       , runInsertReturning
       , runUpdate
       , runUpdate_
       , runDelete

         -- * Runners
       , invoke
       , invokeUnsafe
       , transact

       , anyAffected

         -- * Misc
       , withConnection

         -- * Internal
       , liftPg
       , withTransactionSerializable
       ) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import qualified Control.Exception as E
import Control.Lens (views)
import Control.Monad.Reader (mapReaderT)
import Database.Beam.Backend (FromBackendRow, MonadBeam (..))
import qualified Database.Beam.Backend.SQL.BeamExtensions as Backend
import Database.Beam.Postgres (Pg, PgCommandSyntax, PgDeleteSyntax, PgInsertSyntax, PgSelectSyntax,
                               PgUpdateSyntax, Postgres, runBeamPostgres, runBeamPostgresDebug)
import qualified Database.Beam.Postgres.Conduit as Backend.Conduit
import Database.Beam.Query (QExpr, SqlDelete, SqlInsert, SqlInsertValues, SqlSelect, SqlUpdate)
import qualified Database.Beam.Query as Backend
import Database.Beam.Schema (DatabaseEntity, TableEntity)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as Backend
import Database.PostgreSQL.Simple.Errors as Backend
import qualified Database.PostgreSQL.Simple.Transaction as Backend
import Fmt ((+|), (|+))
import GHC.Stack (popCallStack)
import Loot.Base.HasLens (HasCtx, HasLens (..))
import qualified Loot.Log as Log
import qualified System.Console.ANSI as ANSI
import UnliftIO (MonadUnliftIO (..), UnliftIO (..), askUnliftIO)
import qualified UnliftIO as UIO

import Dscp.Config
import Dscp.DB.SQL.Error
import Dscp.DB.SQL.Types
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
    (ConnectionString connStr, connNum, maxPending, txsSwitch) <- case ppMode params of
        PostgresReal realParams -> do
            let connStr    = realParams ^. option #connString
                mConnNum   = realParams ^. option #connNum
                maxPending = realParams ^. option #maxPending

            connNum <- case mConnNum of
                Nothing  -> liftIO $ max 1 . pred <$> getNumCapabilities
                Just num -> pure num
            return (connStr, connNum, maxPending, RealTransactions)

        PostgresTest PostgresTestParams{ ptpConnString = connStr } ->
            return (connStr, 1, 1000000000, TestTransactions)

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
        , sqlTransactionsSwitch = txsSwitch
        }

closePostgresDB :: MonadIO m => SQL -> m ()
closePostgresDB sd =
    -- we return closed connections back to pool, because pending requests to DB
    -- would better throw an exception trying to operate with closed connection
    -- rather than just hang.
    liftIO $ forEachConnection sd Backend.close

------------------------------------------------------------
-- SQL context
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
newtype DBT (t :: TransactionalContext) m a = DBT
    { runDBT :: ReaderT Connection m a
    } deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch)

-- | Lifted actions should NOT have side-effects as soon as any 'DBT'
-- action can happen multiple times per single 'transact' call.
deriving instance MonadIO m => MonadIO (DBT t m)

instance MonadUnliftIO m => MonadUnliftIO (DBT t m) where
    askUnliftIO = do
        UnliftIO unlift <- DBT askUnliftIO
        return $ UnliftIO $ unlift . runDBT

instance MonadReader r m => MonadReader r (DBT t m) where
    ask = DBT $ lift ask
    reader = DBT . lift . reader
    local doModify = DBT . mapReaderT (local doModify) . runDBT

instance (Log.MonadLogging m, Monad m) => Log.MonadLogging (DBT t m) where
    log = DBT . lift ... Log.log
    logName = DBT $ lift Log.logName

instance (Log.ModifyLogName m, Monad m) => Log.ModifyLogName (DBT t m) where
    modifyLogNameSel how = DBT . mapReaderT (Log.modifyLogNameSel how) . runDBT

liftPg :: MonadIO m => Pg a -> DBT t m a
liftPg action =
    DBT . ReaderT $ \conn ->
        liftIO $ maybe runBeamPostgres runBeamPostgresDebug logger conn action
  where
    logger = Nothing
    -- logger = Just sqlDebugLogger

instance MonadUnliftIO m =>
         MonadBeam PgCommandSyntax Postgres Connection (DBT t m) where
    -- We omit implementation of the following methods as soon as they work in IO only,
    -- use dedicated 'DBT' runners instead ('runInsert' and others).
    withDatabaseDebug = error "DBT.withDatabaseDebug: not implemented"
    withDatabase = error "DBT.withDatabase: not implemented"
    runNoReturn = error "DBT.runNoReturn: not implemented"
    runReturningMany = error "DBT.runReturningMany: not implemented"

-- | Declares whether given 'DBT' actions should be performed within
-- transaction.
data TransactionalContext = WithinTx | OutsideOfTransaction

------------------------------------------------------------
-- SQL queries building
------------------------------------------------------------

{- We rewrite runners as soon as it allows requiring write or transaction context.
-}

-- | Allows to judge about how many changes in a table were introduced by a query.
newtype AffectedRowsNumber = AffectedRowsNumber Word64
    deriving (Show, Eq, Ord, Num)

-- | Convert information about changes made by an updating query.
toAffected :: Int64 -> AffectedRowsNumber
toAffected aff = E.assert (aff >= 0) $ AffectedRowsNumber (fromIntegral aff)

-- | Whether an operation made any changes in database.
anyAffected :: AffectedRowsNumber -> Bool
anyAffected (AffectedRowsNumber aff) = aff > 0

-- | Run select query.
-- We abandon different 'runSelectReturningOne' and 'runSelectReturningList' versions
-- for the sake of better processing of unexpected behavior.
runSelect
    :: (MonadIO m, FromBackendRow Postgres a)
    => SqlSelect PgSelectSyntax a -> DBT t m [a]
runSelect = liftPg . Backend.runSelectReturningList

-- | Run select query and modify fetched results.
runSelectMap
    :: (MonadIO m, FromBackendRow Postgres a)
    => (a -> b) -> SqlSelect PgSelectSyntax a -> DBT t m [b]
runSelectMap f = fmap (map f) . runSelect

runInsert
    :: MonadIO m
    => SqlInsert PgInsertSyntax -> DBT t m ()
runInsert = liftPg . Backend.runInsert

runInsertReturning
    :: (MonadIO m, _)
    => DatabaseEntity be db (TableEntity table)
    -> SqlInsertValues _ (table (QExpr _ _))
    -> DBT t m [table Identity]
runInsertReturning db = liftPg . Backend.runInsertReturningList db

runUpdate
    :: MonadIO m
    => SqlUpdate PgUpdateSyntax tbl -> DBT t m AffectedRowsNumber
runUpdate query =
    DBT . ReaderT $ \conn ->
        toAffected <$> Backend.Conduit.runUpdate conn query

runUpdate_
    :: MonadIO m
    => SqlUpdate PgUpdateSyntax tbl -> DBT t m ()
runUpdate_ = void . runUpdate

runDelete
    :: MonadIO m
    => SqlDelete PgDeleteSyntax tbl -> DBT t m AffectedRowsNumber
runDelete query =
    DBT . ReaderT $ \conn ->
        toAffected <$> Backend.Conduit.runDelete conn query

-- | For low-level operations.
withConnection :: MonadIO m => (Connection -> IO a) -> DBT t m a
withConnection f = DBT $ ReaderT (liftIO . f)

------------------------------------------------------------
-- Transactions
------------------------------------------------------------

{- [Note: sql-transactions-in-tests]

This is said to be a common approach to wrap a test case in a transaction and rollback
the transaction in the end, this way the database schema remains clean (all tables empty)
after each test case.

Nested transactions are not allowed, we should use savepoints instead. Thus when in tests,
we map transactions in the code to savepoints. They (savepoints) are necessary,
otherwise any error will render the entire test-case level transaction invalid.

We allocate only one connection for all tests and execute tests sequentially.
Otherwise test-case level transactions deadlock on each other; rolling back and repeating
them would require a version of 'withTransactionSerializable' which puts the test connection
back to the pool before executing passed action and this looks even more :ssc: than it
currently is.
Anyway, sequential execution does not slow tests down much: all educator tests completion
without lock (few tests fail with deadlock) takes 2:30, with lock - 3:10.
-}

-- | Run transaction with the highest "Serializable" serialization level,
-- repeating on rollbacks until success.
--
-- This slightly differs from 'Backend.withTransactionSerializable', it repeats
-- after wider amount of different error kinds.
withTransactionSerializable
    :: (MonadUnliftIO m, HasCallStack)
    => DBT t m a -> DBT t m a
withTransactionSerializable action = do
    UnliftIO unliftIO <- askUnliftIO
    withConnection $ \conn ->
        Backend.withTransactionModeRetry
            Backend.TransactionMode
                { Backend.isolationLevel = Backend.Serializable
                , Backend.readWriteMode = Backend.ReadWrite
                }
            isTransientError
            conn
            (unliftIO action)
  where
    isTransientError err = or
        [ Backend.isSerializationError err
          ---- ^ Failed to provide serialization guarantees
        , Backend.sqlState err == "40P01"
          ---- ^ Rollback due to deadlock
        ]

-- | Create a savepoint within an active transaction.
withSavepoint :: (MonadUnliftIO m, HasCallStack) => DBT t m a -> DBT t m a
withSavepoint action = do
    UnliftIO unliftIO <- askUnliftIO
    withConnection $ \conn ->
        Backend.withSavepoint conn $ unliftIO action

-- | Apply given function which sets transaction context and reports if
-- that function invokes given action too many times.
countingRollbacks
    :: (MonadUnliftIO m, Log.MonadLogging m, HasCallStack)
    => (m a -> m a) -> m a -> m a
countingRollbacks withTransaction action = do
    callsRef <- newIORef (0 :: Int)
    withTransaction (countCalls callsRef >> action)
  where
    countCalls callsRef = do
        callNo <- atomicModifyIORef' callsRef (\c -> (c + 1, c))
        when ((callNo `mod` 3) == 0 && callNo > 0) $
            Log.logWarning $ "Following transaction has rollbacked " +| callNo |+
                             " times\n" +| (prettyCallStack $ popCallStack callStack) |+ ""

------------------------------------------------------------
-- DBT runners
------------------------------------------------------------

-- | Run 'DBT' without carying about whether it assumes to be run in transaction
-- or not.
invokeUnsafe
    :: (MonadUnliftIO m, HasCtx ctx m '[SQL])
    => DBT t m a -> m a
invokeUnsafe action = do
    mode <- views (lensOf @SQL) sqlTransactionsSwitch
    borrowConnection $ runReaderT (runDBT $ actionWrapper mode action)
  where
    actionWrapper = \case
        RealTransactions -> id
        TestTransactions -> withSavepoint  -- [Note: sql-transactions-in-tests]

-- | Run 'DBT'.
invoke
    :: (MonadUnliftIO m, HasCtx ctx m '[SQL])
    => DBT 'OutsideOfTransaction m a -> m a
invoke = invokeUnsafe

-- | Run DBT action within a transaction.
-- Note that action may happen multiple times.
transact
    :: forall t ctx m a.
       (MonadUnliftIO m, Log.MonadLogging m, HasCtx ctx m '[SQL], HasCallStack)
    => RequiresTransaction t => DBT t m a -> m a
transact action = do
    mode <- views (lensOf @SQL) sqlTransactionsSwitch
    borrowConnection $ \conn ->
        runReaderT (runDBT $ inTransaction mode action) conn
  where
    inTransaction = \case
        RealTransactions -> countingRollbacks withTransactionSerializable
        TestTransactions -> countingRollbacks withSavepoint

-- | Helps to prevent using 'transact' when 'invoke' is enough.
class RequiresTransaction (t :: TransactionalContext)
instance RequiresTransaction 'WithinTx

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

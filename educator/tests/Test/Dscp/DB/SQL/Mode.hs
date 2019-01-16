module Test.Dscp.DB.SQLite.Mode
    ( PostgresTestServer (..)
    , testPostgresParams
    , specWithTempPostgresServer
    , withPostgresDb
    , runPostgresMode
    ) where

import Control.Concurrent.MVar (withMVar)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (IsolationLevel (..), ReadWriteMode (..),
                                               TransactionMode (..))
import qualified Database.PostgreSQL.Simple.Transaction as Transaction
import GHC.IO.Unsafe (unsafePerformIO)
import Loot.Log (LoggingIO)
import System.Environment (lookupEnv)
import Test.Hspec.Core.Hooks (beforeAll)
import UnliftIO (MonadUnliftIO, UnliftIO (..), askUnliftIO)

import Test.Hspec.Core.Spec (Spec, SpecWith)

import Dscp.DB.SQLite
import Dscp.Educator.DB
import Dscp.Rio
import Dscp.Util.Test

-- | Information about postgres test server.
data PostgresTestServer = PostgresTestServer
    { ptsConnString :: ConnectionString
      -- ^ Way to connect to test server.
    , ptsWithLock   :: forall m a. MonadUnliftIO m => m a -> m a
      -- ^ Lock on database access.
      -- Each test case should acquire it before performing database operations.
      -- [Note: sql-transactions-in-tests]
    }

-- | Test parameters for db which is stored in filesystem.
testPostgresParams :: ConnectionString -> PostgresParams
testPostgresParams connStr = PostgresParams
    { ppMode = PostgresTest PostgresTestParams
        { ptpConnString = connStr
        }
    }

-- | Run an action with connection.
-- In tests we have only one connection and access to the database is protected with lock,
-- thus it is safe to use this function for small operations like @BEGIN TRANSACTION@.
withTestConn :: MonadIO m => SQL -> (Connection -> IO a) -> m a
withTestConn db act = runRIO db $ borrowConnection $ liftIO . act

-- | Transaction mode used in tests.
testTxMode :: TransactionMode
testTxMode = TransactionMode Serializable ReadWrite

----------------------------------------------------------------------------
-- Temporal server creation
----------------------------------------------------------------------------

postgresTestServerEnvName :: String
postgresTestServerEnvName = "TEST_PG_CONN_STRING"

-- | Fetch all information about the Postgres server we should connect to and
-- prepare schema.
-- Using 'unsafePerformIO' to prepare schema, since doing the specs discovery manually
-- (in order to execute this action there at start) is too tedious.
postgresTestServerCoodrinates :: PostgresTestServer
postgresTestServerCoodrinates = unsafePerformIO $ do
    mRes <- liftIO $ lookupEnv postgresTestServerEnvName
    connStr <- case mRes of
        Nothing -> error $ "Connection string for test server is not provided. \
                           \Pass it via " <> show postgresTestServerEnvName <>
                           " environmental variable."
        Just res -> do
            when (null res) $
                putTextLn "Warning: empty connection string to postgres server specified"
            return (connStringFromText res)

    lock <- newMVar ()

    let server = PostgresTestServer
            { ptsConnString = connStr
            , ptsWithLock = \action -> do
                  UnliftIO unliftIO <- askUnliftIO
                  liftIO $ withMVar lock $ \() -> unliftIO action
            }

    withPostgresDb server $ \_rollbackInEnd db ->
        runRIO testLogging $ withTransaction db $ prepareEducatorSchema db
    return server
  where
    withTransaction db =
        bracket_ (withTestConn db $ Transaction.beginMode testTxMode)
                 (withTestConn db Transaction.commit)
{-# NOINLINE postgresTestServerCoodrinates #-}

-- | You probably want to start all your database-related specs with this function.
-- It will provide 'PostgresTempDB' argument to properties specified in each 'it' call
-- of that spec tree.
-- TODO [DSCP-417]: Since 'postgresTestServerCoordinates' is a constant, it potentially can
-- be passed directly rather than via spec tree. Although it's not clear yet whether
-- this function will be implemented as a constant in future as well, so leaving this
-- refactoring for better times.
specWithTempPostgresServer :: HasCallStack => SpecWith PostgresTestServer -> Spec
specWithTempPostgresServer = beforeAll (evaluateWHNF postgresTestServerCoodrinates)

----------------------------------------------------------------------------
-- Connection to a database
----------------------------------------------------------------------------

{- Further we assume, that database user credentials are passed via the environment.
   We will run test cases sequentially with database provided via environment.
-}

-- | Action which wraps computation into an SQL transaction and rollbacks the transaction
-- in the end, so that next time we start with a clear schema.
-- [Note: sql-transactions-in-tests]
type SetTestTransaction a = IO a -> IO a

-- | Connect to a database in the given test SQL server and run an action with it.
withPostgresDb :: PostgresTestServer -> (SetTestTransaction a -> SQL -> IO a) -> IO a
withPostgresDb pts action =
    bracket (openPostgresDB params) closePostgresDB $ \db ->
        action (setTx db) db
  where
    params = testPostgresParams (ptsConnString pts)
    setTx db =
        bracket_ (withTestConn db $ Transaction.beginMode testTxMode)
                 (withTestConn db Transaction.rollback)
      . ptsWithLock pts

-- | Run an action with a context supplied, using the given test SQL server.
runPostgresMode :: PostgresTestServer -> RIO (SQL, LoggingIO) a -> IO a
runPostgresMode testDb action =
    withPostgresDb testDb $ \rollbackInEnd db ->
        rollbackInEnd $ runRIO (db, testLogging) action

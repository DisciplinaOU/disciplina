module Test.Dscp.DB.SQLite.Mode
    ( PostgresTestServer
    , testRealPostgresParams
    , specWithTempPostgresServer
    , withPostgresDb
    , runPostgresMode
    ) where

import qualified Database.PostgreSQL.Simple.Transaction as Transaction
import System.Environment (lookupEnv)
import Test.Hspec.Core.Hooks (beforeAll)
import Test.Hspec.Core.Spec (Spec, SpecWith)

import Dscp.DB.SQLite
import Dscp.Rio

-- | Information about postgres test server.
newtype PostgresTestServer = PostgresTestServer ConnectionString

-- | Test parameters for db which is stored in filesystem.
testRealPostgresParams :: ConnectionString -> PostgresParams
testRealPostgresParams connStr = PostgresParams
    { ppMode = PostgresTest PostgresTestParams
        { ptpConnString = connStr
        }
    }

----------------------------------------------------------------------------
-- Temporal server creation
----------------------------------------------------------------------------

postgresTestServerEnvName :: String
postgresTestServerEnvName = "TEST_PG_CONN_STRING"

fetchPostgresTestServer :: MonadIO m => m PostgresTestServer
fetchPostgresTestServer = do
    mRes <- liftIO $ lookupEnv postgresTestServerEnvName
    case mRes of
        Nothing -> error $ "Connection string for test server is not provided. \
                           \Pass it via " <> show postgresTestServerEnvName <>
                           " environmental variable."
        Just res -> do
            when (null res) $
                putTextLn "Warning: empty connection string to postgres server specified"
            return (PostgresTestServer $ connStringFromText res)

-- You probably want to start all your database-related specs with this function.
-- It will provide 'PostgresTempDB' argument to properties specified in each 'it' call
-- of that spec tree.
specWithTempPostgresServer :: SpecWith PostgresTestServer -> Spec
specWithTempPostgresServer = beforeAll fetchPostgresTestServer

----------------------------------------------------------------------------
-- Connection to a database
----------------------------------------------------------------------------

{- Further we assume, that database user credentials are passed via the environment.
   We will run test cases sequentially with database provided via environment.
-}

-- | Connect to a database in the given test SQL server and run an action with it.
withPostgresDb :: PostgresTestServer -> (SQL -> IO a) -> IO a
withPostgresDb (PostgresTestServer connStr) action =
    bracket (openPostgresDB params) closePostgresDB $ \db ->
        bracket_ (withConn db Transaction.begin) (withConn db Transaction.rollback)
        (action db)
  where
    params = testRealPostgresParams connStr
    withConn db act = runRIO db $ borrowConnection $ liftIO . act

-- | Run an action with a context supplied, using the given test SQL server.
runPostgresMode :: PostgresTestServer -> RIO SQL a -> IO a
runPostgresMode testDb action = withPostgresDb testDb $ flip runRIO action

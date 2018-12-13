module Test.Dscp.DB.SQLite.Mode
    ( PostgresTestServer
    , testRealPostgresParams
    , specWithTempPostgresServer
    , allocPostgresDb
    , runPostgresMode
    ) where

import Control.Exception.Safe (handle)
import qualified Data.List as L
import qualified Database.Postgres.Temp as PostgresTemp
import GHC.IO.Unsafe (unsafePerformIO)
import System.Directory.Internal.Prelude (isDoesNotExistError)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec.Core.Hooks (afterAll, beforeAll)
import Test.Hspec.Core.Spec (Spec, SpecWith)

import Dscp.DB.SQLite
import Dscp.Rio
import Dscp.Util

type PostgresTestServer = PostgresTemp.DB

-- | Test parameters for db which is stored in filesystem.
testRealPostgresParams :: ConnectionString -> PostgresParams
testRealPostgresParams connStr = PostgresParams
    { ppMode = PostgresReal PostgresRealParams
        { prpConnString = connStr
        , prpConnNum = Just 5
        , prpMaxPending = 1000
        }
    }

----------------------------------------------------------------------------
-- Temporal server creation
----------------------------------------------------------------------------

-- | Run a temporal postgres server.
--
-- Database creation is a heavyweight operation (takes several seconds), consider
-- using it once in a test tree definition.
--
-- This function is entirely Unix thing inside, although providing support for other
-- systems should not take too much effort.
--
-- You can find some useful logs, maybe even including database requests,
-- in '/tmp/tmp-postgresXXXXX/' directory.
startTempPostgresServer :: IO PostgresTestServer
startTempPostgresServer =
    leftToThrow id =<< handleNoDbInit (PostgresTemp.startAndLogToTmp [])
  where
    handleNoDbInit = handle $ \e ->
        if isDoesNotExistError e && ("initdb" `L.isInfixOf` show e)
        then error "No 'initdb' found. Have you added '/usr/lib/postgresql/VERSION/bin/' \
                   \to PATH?"
        else throwM e

-- | Stop the postgres server.
closeTempPostgresServer :: PostgresTestServer -> IO ()
closeTempPostgresServer db = do
    exitCode <- PostgresTemp.stop db
    case exitCode of
        ExitSuccess -> pass
        ExitFailure code -> putTextLn $ "Temporal postgres server closed abnormally \
                                        \(code " <> show code <> ")"

-- | Run a temporal postgres server for the given spec tree.
--
-- You probably want to start all your database-related specs with this function.
-- It will provide 'PostgresTempDB' argument to properties specified in each 'it' call
-- of that spec tree.
specWithTempPostgresServer :: SpecWith PostgresTestServer -> Spec
specWithTempPostgresServer =
    -- TODO: 'afterAll' produces "unnamed" node in tree, why so?
    beforeAll startTempPostgresServer . afterAll closeTempPostgresServer

----------------------------------------------------------------------------
-- Connection to a database
----------------------------------------------------------------------------

testDatabaseCounter :: TVar Integer
testDatabaseCounter = unsafePerformIO $ newTVarIO 0
{-# NOINLINE testDatabaseCounter #-}

-- | Generate a unique identifier number for a database.
genDatabaseId :: MonadIO m => m Integer
genDatabaseId =
    atomically $ readTVar testDatabaseCounter <* modifyTVar' testDatabaseCounter (+1)

{- Further we assume, that database user credentials are passed via the environment.
   We assume that one database will be created for every single test case.
-}

-- | Extracts parameters like "host" and "post" from a connection string in URL form
-- (which is provided by 'PostgresTemp.startAndLogToTmp' call).
connStringParams :: ConnectionString -> [String]
connStringParams (ConnectionString connStr) =
    let [_, params] BS.split (bsChar '?') connStr
    in BS.split (bsChar '&') connStr
  where
    bsChar = toEnum . fromEnum

-- | Creates a database with the given name.
createPostgresDb :: MonadIO m => ConnectionString -> m ()
createPostgresDb connStr = do
    let params = map ("--" <>) connStringParams connStr
    (code, _out, err) <- liftIO $ readProcessWithExitCode "createdb" (dbName : params) ""
    case code of
        ExitSuccess   -> pass
        ExitFailure _ -> error $ "Database creation failed: " <> toText err

-- | Creates a database with the given name.
destroyPostgresDb :: MonadIO m => String -> m ()
destroyPostgresDb dbName = do
    let params = map ("--" <>) connStringParams connStr
    (code, _out, err) <- liftIO $ readProcessWithExitCode "dropdb" (dbName : params) ""
    case code of
        ExitSuccess   -> pass
        ExitFailure _ -> error $ "Database drop failed: " <> toText err

-- | Allocate a database in the given test SQL server and run an action with it.
-- We assume that
allocPostgresDb :: PostgresTestServer -> (SQL -> IO a) -> IO a
  allocPostgresDb testDb action =
    bracket openUniquePostgresDB demolishPostgresDB $ \(_, db) -> action db
  where
    openUniquePostgresDB = do
        did <- genDatabaseId
        traceM $ "Opening unique database " <> show did
        let baseConnStr = connStringFromText (PostgresTemp.connectionString testDb)
            dbName = (baseConnStr ^. connStringDatabaseL) <> show did
            connStr = baseConnStr & connStringDatabaseL .~ dbName
        traceM $ "Creating db: " <> show dbName
        createPostgresDb (decodeUtf8 dbName)
        traceM $ "Opening db: " <> show connStr
        db <- openPostgresDB (testRealPostgresParams connStr)
        return (decodeUtf8 dbName, db)
    demolishPostgresDB (dbName, db) = do
        closePostgresDB db
        destroyPostgresDb dbName

-- | Run an action with a context supplied, using the given test SQL server.
runPostgresMode :: PostgresTestServer -> RIO SQL a -> IO a
runPostgresMode testDb action = allocPostgresDb testDb $ flip runRIO action

module Test.Dscp.DB.SQLite.Real.Mode
    ( testRealPostgresParams
    , withTempPostgres
    , runPostgresMode
    , launchPostgresMode
    ) where

import Control.Exception.Safe (handle)
import qualified Data.List as L
import qualified Database.Postgres.Temp as PostgresTemp
import System.Directory.Internal.Prelude (isDoesNotExistError)
import System.Exit (ExitCode (..))

import Dscp.DB.SQLite
import Dscp.Rio
import Dscp.Util

-- | Test parameters for db which is stored in filesystem.
testRealPostgresParams :: PostgresTemp.DB -> PostgresParams
testRealPostgresParams testDb = PostgresParams
    { ppMode = PostgresReal PostgresRealParams
        { prpConnString = fromString $ PostgresTemp.connectionString testDb
        , prpConnNum = Just 5
        , prpMaxPending = 1000
        }
    }

-- | Run a temporal postgres server
-- This is entirely Unix thing inside, although providing support for other systems
-- would not take too much effort.
withTempPostgres :: (PostgresTemp.DB -> IO a) -> IO a
withTempPostgres = bracket alloc destroy
  where
    alloc = leftToThrow id =<< handleNoDbInit (PostgresTemp.start [])
    destroy db = do
        exitCode <- PostgresTemp.stop db
        case exitCode of
            ExitSuccess -> pass
            ExitFailure code -> putTextLn $ "Temporal postgres server closed abnormally \
                                            \(code " <> show code <> ")"
    handleNoDbInit = handle $ \e ->
        if isDoesNotExistError e && ("initdb" `L.isInfixOf` show e)
        then error "No 'initdb' found. Have you added '/usr/lib/postgresql/VERSION/bin/' \
                   \to PATH?"
        else throwM e

-- | Run an action with a context supplied, database would refer to the given test 'DB'
-- parameters.
runPostgresMode :: PostgresTemp.DB -> RIO SQL a -> IO a
runPostgresMode testDb action = do
    bracket (openPostgresDB dbParams)
            (closePostgresDB) $
            \sql -> runRIO sql action
  where
    dbParams = testRealPostgresParams testDb

-- | Run an action launching a temporal database server for it.
launchPostgresMode :: RIO SQL a -> IO a
launchPostgresMode action = withTempPostgres $ \testDb -> runPostgresMode testDb action

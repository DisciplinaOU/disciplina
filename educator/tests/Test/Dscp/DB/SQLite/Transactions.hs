{-# LANGUAGE QuasiQuotes #-}

-- | This module tests that our wrapper over SQLite library
-- allow SQL transactions to work properly.
-- Transactions do not work as expected automatically; for instances,
-- queries are not allowed to be performed concurrently via the same connection.
module Test.Dscp.DB.SQLite.Transactions where

import qualified Control.Concurrent.STM as STM
import qualified Data.List as L
import Database.Beam.Migrate (CheckedDatabaseSettings, defaultMigratableDbSettings, unCheckDatabase)
import Database.Beam.Migrate.Simple (createSchema)
import Database.Beam.Schema (Beamable, C, Database, Table (..), TableEntity)
import Database.SQLite.Simple (Only (..), execute, query)
import Loot.Base.HasLens (HasCtx)
import System.Directory (removeFile)
import Text.InterpolatedString.Perl6 (q)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as UIO

import Dscp.DB.SQLite
import Dscp.DB.SQLite.Util
import Dscp.Rio
import Dscp.Util
import Dscp.Util.Test

type MonadMoney m = (MonadIO m, MonadCatch m, MonadUnliftIO m)

type Money = Int

data AccountRowT f = AccountRowT
    { arMoney :: C f Money
    } deriving (Generic)

data AccountSchema f = AccountSchema
    { asAccounts :: f (TableEntity AccountRowT)
    } deriving (Generic)

instance Table AccountRowT where
    newtype PrimaryKey AccountRowT f = AccountRowId (C f Money)
        deriving (Generic)

instance Beamable AccountRowT
instance Beamable (PrimaryKey AccountRowT)

accountsCheckedSchema :: CheckedDatabaseSettings be AccountSchema
accountsCheckedSchema = defaultMigratableDbSettings

accountsSchema :: Database be AccountSchema => DatabaseSettings be AccountSchema
accountsSchema = unCheckDatabase accountsCheckedSchema


prepareSchema :: (MonadIO m, HasConnection m) => m ()
prepareSchema = do
    createSchema _ accountsCheckedSchema
    runInsert . insert (asAccounts accountsSchema) $
        insertValue 0

getMoney
    :: forall cmd be hdl m.
       Database be AccountSchema
    => MonadQuery cmd be hdl m =>
       m Money
getMoney =
    fmap oneOrError . runSelect . select $ do
        arMoney <$> all_ (asAccounts $ accountsSchema @be)

setMoney
    :: forall cmd be hdl m.
       (Database be AccountSchema, MonadQuery cmd be hdl m, WithinWrite)
    => Money -> m ()
setMoney val =
    runUpdate $ update
        (asAccounts $ accountsSchema @be)
        (\acc -> [ arMoney acc <-. val_ val ])
        (\_ -> val_ True)

addMoney :: (MonadUnliftIO m, HasCtx ctx m '[SQLiteDB]) => m ()
addMoney =
    transactW $ do
        money <- getMoney
        setMoney (money + 1)

runSQLiteMode :: RIO SQLiteDB a -> IO a
runSQLiteMode action = do
    db' <- openSQLiteDB dbParams
    runRIO db' $ invoke prepareSchema
    closeSQLiteDB db'
    bracket (openSQLiteDB dbParams)
            (\db -> closeSQLiteDB db >> removeFile dbPath) $
            \db -> runRIO db action
  where
    -- sad, but testing with in-memory database is not an option
    -- because each connection would work with its own database in this case
    dbPath = "./sql-transaction-test.db"
    dbParams = SQLiteParams
        { sdpMode = SQLiteReal SQLiteRealParams
            { srpPath = dbPath
            , srpConnNum = Just 5
            , srpMaxPending = 1000
            }
        }

spec_SQLiteWrapper :: Spec
spec_SQLiteWrapper = do
    it "SQLite wrapper thread-safety" . property . ioProperty . runSQLiteMode $ do
        invoke prepareSchema
        let iterations = 100

        started <- newTVarIO False
        workers <- forM [1 :: Int .. iterations] $ \_ ->
            UIO.async $ do
                atomically $ readTVar started >>= STM.check
                addMoney

        atomically $ writeTVar started True
        forM workers UIO.waitCatch >>= mapM_ (either throwM pure)

        money <- invoke getMoney
        return $ money === iterations

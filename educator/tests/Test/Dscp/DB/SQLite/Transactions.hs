{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes      #-}

-- | This module tests that our wrapper over SQL library
-- allow SQL transactions to work properly.
-- Transactions do not work as expected automatically; for instances,
-- queries are not allowed to be performed concurrently via the same connection.
module Test.Dscp.DB.SQLite.Transactions where

import qualified Control.Concurrent.STM as STM
import Control.Lens ((?~))
import Database.Beam.Migrate (CheckedDatabaseSettings, defaultMigratableDbSettings, unCheckDatabase)
import Database.Beam.Migrate.Simple (createSchema)
import Database.Beam.Postgres (PgCommandSyntax, Postgres)
import Database.Beam.Postgres.Migrate (migrationBackend)
import Database.Beam.Schema.Tables (Beamable, C, Database, DatabaseSettings, Table (..),
                                    TableEntity)
import Loot.Base.HasLens (HasCtx)
import Loot.Config (finaliseDeferredUnsafe, option)
import Loot.Log (LoggingIO, MonadLogging)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as UIO

import Dscp.DB.SQLite
import Dscp.Rio
import Dscp.Util
import Dscp.Util.Test

import Test.Dscp.DB.SQLite.Mode

type MonadMoney m = (MonadIO m, MonadCatch m, MonadUnliftIO m)

type Money = Int

data AccountRowT f = AccountRow
    { arMoney   :: C f Money
    } deriving (Generic)

type AccountRow = AccountRowT Identity

instance Table AccountRowT where
    newtype PrimaryKey AccountRowT f = AccountRowId (C f Int)
        deriving (Generic)
    primaryKey = AccountRowId . arMoney

instance Beamable AccountRowT
instance Beamable (PrimaryKey AccountRowT)

data BankSchema f = BankSchema
    { bsAccounts :: f (TableEntity AccountRowT)
    } deriving (Generic)

instance Database be BankSchema

bankCheckedSchema :: CheckedDatabaseSettings Postgres BankSchema
bankCheckedSchema = defaultMigratableDbSettings @PgCommandSyntax

bankSchema :: DatabaseSettings Postgres BankSchema
bankSchema = unCheckDatabase bankCheckedSchema

prepareBankSchema :: MonadIO m => DBT 'WithinTx m ()
prepareBankSchema = do
    liftPg $ createSchema migrationBackend bankCheckedSchema
    runInsert . insert (bsAccounts bankSchema) $ insertValue (AccountRow 0)

getMoney :: MonadIO m => DBT t m Money
getMoney = fmap oneOrError . runSelectMap arMoney . select $ all_ (bsAccounts bankSchema)

setMoney :: MonadIO m => Money -> DBT t m ()
setMoney val =
    runUpdate_ $ update
        (bsAccounts bankSchema)
        (\acc -> [ arMoney acc <-. val_ val ])
        (\_ -> val_ True)

addMoney :: (MonadUnliftIO m, MonadLogging m, HasCtx ctx m '[SQL]) => m ()
addMoney =
    transact @'WithinTx $ do
        money <- getMoney
        setMoney (money + 1)

runRealPostgresMode :: PostgresTestServer -> RIO (SQL, LoggingIO) a -> IO a
runRealPostgresMode testDb action =
    bracket (openPostgresDB params) closePostgresDB $ \db ->
        runRIO (db, testLogging) action
  where
    params = PostgresParams
        { ppMode = PostgresReal . finaliseDeferredUnsafe $ mempty
            & option #connString ?~ ptsConnString testDb
            & option #connNum ?~ Just 10
            & option #maxPending ?~ 100000
        }

spec_SQLiteWrapper :: Spec
spec_SQLiteWrapper = specWithTempPostgresServer $ do
    it "SQLite wrapper thread-safety" $ \testDb -> ioProperty . runPostgresMode testDb $ do
        transact prepareBankSchema
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

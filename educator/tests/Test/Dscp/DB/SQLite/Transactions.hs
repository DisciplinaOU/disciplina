{-# LANGUAGE QuasiQuotes #-}

-- | This module tests that our wrapper over SQLite library
-- allow SQL transactions to work properly.
-- Transactions do not work as expected automatically; for instances,
-- queries are not allowed to be performed concurrently via the same connection.
module Test.Dscp.DB.SQLite.Transactions where

import qualified Control.Concurrent.STM as STM
import qualified Data.List as L
import Database.SQLite.Simple (Only (..), execute, query)
import Loot.Base.HasLens (HasCtx)
import System.Directory (removeFile)
import Text.InterpolatedString.Perl6 (q)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as UIO

import Dscp.DB.SQLite
import Dscp.Rio
import Dscp.Util.Test

type MonadMoney m = (MonadIO m, MonadCatch m, MonadUnliftIO m)

type Money = Int

prepareSchema :: (MonadIO m) => m ()
prepareSchema =
    forM_ [createTableQuery, addAccountQuery] $
        \que -> withConnection $ \conn -> execute conn que ()
  where
    createTableQuery = [q|
        create table if not exists Accounts (
            amount    INTEGER
        );
        |]
    addAccountQuery = [q|
        insert into Accounts values (0);
        |]

getMoney :: MonadIO m => m Money
getMoney = withConnection $ \conn ->
           fromOnly . L.head <$> query conn queryText ()
  where
    queryText = [q|
        select amount from Accounts
    |]

setMoney :: MonadIO m => Money -> m ()
setMoney val = withConnection $ \conn -> execute conn queryText (Only val)
  where
    queryText = [q|
        update Accounts set amount = ?
    |]

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

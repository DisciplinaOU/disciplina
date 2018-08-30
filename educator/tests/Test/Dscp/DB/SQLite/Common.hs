module Test.Dscp.DB.SQLite.Common
  ( module Test.Dscp.DB.SQLite.Common
  , module Dscp.Core
  , module Dscp.Util
  , module Dscp.Util.Test
  , module Test.Hspec
  , hash
  ) where

import Prelude hiding (fold)

import qualified Data.Text.Buildable
import Database.SQLite.Simple (Connection, execute, fold, query, setTrace, withConnection,
                               withTransaction)
import Fmt ((+|), (+||), (|+), (||+))
import qualified Loot.Log as Log
import Test.Hspec
import qualified Text.Show
import UnliftIO (MonadUnliftIO)

import Dscp.Core
import Dscp.Crypto (hash)
import qualified Dscp.DB.SQLite.Class as Adapter
import Dscp.DB.SQLite.Schema (ensureSchemaIsSetUp)
import Dscp.Educator.Arbitrary ()
import Dscp.Util (idOf)
import Dscp.Util.Test

type Trololo m = (MonadThrow m, MonadCatch m)

-- import System.Directory (removeFile)
-- import System.IO.Error (IOError, isDoesNotExistError)
newtype TestSQLiteM a = TestSQLiteM
  { getTestSQLiteM :: ReaderT Connection IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadFail
             , MonadReader Connection
             , MonadUnliftIO
             )

runTestSQLiteM :: TestSQLiteM a -> IO a
runTestSQLiteM action = do
  let filename = ":memory:"
    -- removeFile filename `catch` \(e :: IOError) -> do
    --    if isDoesNotExistError e
    --    then return ()
    --    else throwM e
  withConnection filename $ \conn -> do
    ensureSchemaIsSetUp conn
    getTestSQLiteM action `runReaderT` conn

sqliteProperty ::
     (Testable prop, Show a)
  => Arbitrary a =>
       (a -> TestSQLiteM prop) -> Property
sqliteProperty action =
  property $ \input -> ioProperty $ do runTestSQLiteM (action input)

instance Adapter.MonadSQLiteDB TestSQLiteM where
  query theQuery args =
    TestSQLiteM $ ReaderT $ \conn -> query conn theQuery args
  execute theRequest args =
    TestSQLiteM $ ReaderT $ \conn -> execute conn theRequest args
  queryStreamed theQuery args seed op =
    TestSQLiteM $
    ReaderT $ \conn ->
      fold conn theQuery args seed $ \a b ->
        getTestSQLiteM (op a b) `runReaderT` conn
  transaction (TestSQLiteM (ReaderT actor)) = do
    TestSQLiteM $ ReaderT $ \conn -> conn `withTransaction` do actor conn
  traced (TestSQLiteM (ReaderT actor)) = do
    TestSQLiteM $
      ReaderT $ \conn -> do
        setTrace conn (Just putStrLn)
        res <- actor conn
        setTrace conn (Nothing)
        return res

-- | When warning or error are logged, this exception is thrown.
data TestLoggedError = TestLoggedError
    { tleLvl :: Log.Level
    , tleMsg :: Text
    }

instance Exception TestLoggedError

instance Show TestLoggedError where
    show = toString . pretty

instance Buildable TestLoggedError where
    build TestLoggedError{..} =
        "Bad situation was logged with level " +|| tleLvl ||+ ": " +| tleMsg |+ ""

instance Log.MonadLogging TestSQLiteM where
  log lvl _ msg =
      when (lvl == Log.Warning || lvl == Log.Error) $
          throwM $ TestLoggedError lvl msg
  logName = return $ error "Logger name requested in test"

instance Log.ModifyLogName TestSQLiteM where
  modifyLogNameSel _ = identity

orIfItFails :: MonadCatch m => m a -> a -> m a
orIfItFails action instead = do
  action `catch` \(_e :: SomeException) -> do return instead

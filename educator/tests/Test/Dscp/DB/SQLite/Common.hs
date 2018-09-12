module Test.Dscp.DB.SQLite.Common
  ( module Test.Dscp.DB.SQLite.Common
  , module Dscp.Core
  , module Dscp.Util
  , module Dscp.Util.Test
  , module Test.Hspec
  , hash
  ) where

import Prelude hiding (fold)

import Control.Lens (makeLenses)
import Loot.Base.HasLens (HasLens (..))
import qualified Loot.Log as Log
import Test.Hspec
import Test.QuickCheck (ioProperty)
import Test.QuickCheck.Monadic (PropertyM, monadic, stop)

import Dscp.Core
import Dscp.Crypto (hash)
import Dscp.DB.SQLite
import Dscp.Educator.Arbitrary ()
import Dscp.Resource.SQLite
import Dscp.Rio
import Dscp.Util (idOf)
import Dscp.Util.Test

type Trololo m = (MonadThrow m, MonadCatch m)

data TestSQLiteCtx = TestSQLiteCtx
    { _tqcDb      :: SQLiteDB
    , _tqcLogging :: Log.Logging IO
    }
makeLenses ''TestSQLiteCtx

type TestSQLiteM = RIO TestSQLiteCtx

runTestSQLiteM :: TestSQLiteM a -> IO a
runTestSQLiteM action = do
    bracket openDB closeSQLiteDB $ \_tqcDb -> do
        let ctx = TestSQLiteCtx{..}
            _tqcLogging = testLogging
        runRIO ctx action
  where
    dbParams = SQLiteParams SQLiteInMemory
    openDB = do
        db <- openSQLiteDB dbParams
        prepareEducatorSchema db
        return db

instance HasLens SQLiteDB TestSQLiteCtx SQLiteDB where
    lensOf = tqcDb
instance HasLens (Log.Logging IO) TestSQLiteCtx (Log.Logging IO) where
    lensOf = tqcLogging

educatorProperty
    :: (Testable prop, Show a, Arbitrary a)
    => (a -> TestSQLiteM prop) -> Property
educatorProperty action =
    property $ \input -> ioProperty $ runTestSQLiteM (action input)

educatorPropertyM :: Testable prop => PropertyM TestSQLiteM prop -> Property
educatorPropertyM action =
    monadic (ioProperty . runTestSQLiteM) (void $ action >>= stop)

sqliteProperty
    :: (Testable prop, Show a, Arbitrary a)
    => (a -> DBT r TestSQLiteM prop) -> Property
sqliteProperty action =
    educatorProperty (invokeUnsafe . action)

sqlitePropertyM :: Testable prop => PropertyM (DBT r TestSQLiteM) prop -> Property
sqlitePropertyM action =
    monadic (ioProperty . runTestSQLiteM . invokeUnsafe) (void $ action >>= stop)

orIfItFails :: MonadCatch m => m a -> a -> m a
orIfItFails action instead = do
  action `catch` \(_e :: SomeException) -> do return instead

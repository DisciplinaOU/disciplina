module Test.Dscp.Educator.Mode
  ( module Test.Dscp.Educator.Mode
  , module Dscp.Core
  , module Dscp.Util
  , module Dscp.Util.Test
  , module Test.Hspec
  , hash
  ) where

import Prelude hiding (fold)

import Control.Lens (makeLenses)
import qualified Loot.Log as Log
import Test.Hspec
import Test.QuickCheck (ioProperty, resize)
import Test.QuickCheck.Monadic (PropertyM, monadic, stop)

import Dscp.Config.Util
import Dscp.Core
import Dscp.Crypto
import Dscp.DB.CanProvideDB as DB
import Dscp.DB.CanProvideDB.Pure as PureDB
import Dscp.DB.SQLite
import Dscp.Educator.Arbitrary ()
import Dscp.Educator.Config
import Dscp.Educator.DB.Resource
import Dscp.Educator.Launcher
import Dscp.Educator.TestConfig
import Dscp.Resource.Keys
import Dscp.Rio
import Dscp.Util
import Dscp.Util.HasLens
import Dscp.Util.Test
import Dscp.Witness

import Test.Dscp.DB.SQLite.Real.Mode

type Trololo m = (MonadThrow m, MonadCatch m)

data TestEducatorCtx = TestEducatorCtx
    { _tecEducatorDb       :: SQL
    , _tecWitnessDb        :: DB.Plugin
    , _tecKeys             :: KeyResources EducatorNode
    , _tecWitnessKeys      :: KeyResources WitnessNode
    , _tecWitnessVariables :: TestWitnessVariables
    , _tecLogging          :: Log.Logging IO
    }
makeLenses ''TestEducatorCtx
deriveHasLensDirect ''TestEducatorCtx

deriveHasLens 'tecWitnessVariables ''TestEducatorCtx ''WitnessVariables
deriveHasLens 'tecWitnessVariables ''TestEducatorCtx ''TestWitnessVariables

type TestEducatorM = RIO TestEducatorCtx

runTestSQLiteM :: TestEducatorM a -> IO a
runTestSQLiteM action =
    withEducatorConfig testEducatorConfig $
    withWitnessConfig (rcast testEducatorConfig) $
    withTempPostgres $ \testDb ->
    bracket (openDB testDb) closePostgresDB $ \db ->
    runRIO _tecLogging $ do
        _tecWitnessKeys <- mkCommitteeStore (CommitteeParamsOpen 0)
        _tecWitnessDb <- PureDB.plugin <$> liftIO PureDB.newCtxVar
        _tecWitnessVariables <- mkTestWitnessVariables (_tecWitnessKeys ^. krPublicKey)
                                                       _tecWitnessDb
        let _tecKeys = KeyResources $ mkSecretKeyData testSomeGenesisSecret
        let _tecEducatorDb = db
        let ctx = TestEducatorCtx{..}
        runRIO ctx $ do
              markWithinWriteSDLockUnsafe applyGenesisBlock
              action
  where
    openDB testDb = do
        db <- openPostgresDB (testRealPostgresParams testDb)
        prepareEducatorSchema db
        return db
    _tecLogging = testLogging

educatorPropertyM
    :: Testable prop
    => ((HasEducatorConfig, WithinTx) => PropertyM TestEducatorM prop)
    -> Property
educatorPropertyM action =
    monadic (ioProperty . runTestSQLiteM)
            (withEducatorConfig testEducatorConfig $ void $ allowTxUnsafe action >>= stop)

educatorProperty
    :: (Testable prop, Show a, Arbitrary a)
    => (HasEducatorConfig => a -> TestEducatorM prop)
    -> Property
educatorProperty action =
    educatorPropertyM $ pick (resize 5 arbitrary) >>= lift . action

sqliteProperty
    :: (Testable prop, Show a, Arbitrary a, MonadQuery qm)
    => (WithinTx => TestEducatorCtx -> a -> qm prop) -> Property
sqliteProperty action =
    educatorProperty $ \input -> do
        ctx <- ask
        invokeUnsafe (action ctx input)

sqlitePropertyM
    :: (MonadQuery qm, Testable prop)
    => (WithinTx => TestEducatorCtx -> PropertyM qm prop) -> Property
sqlitePropertyM action =
    monadic (\getProperty -> ioProperty . runTestSQLiteM $
                ask >>= \ctx -> invoke (runReaderT getProperty ctx)) $
            do
              ctx <- lift ask
              res <- hoistPropertyM lift (`runReaderT` ctx) $ allowTxUnsafe (action ctx)
              void $ stop res

orIfItFails :: MonadCatch m => m a -> a -> m a
orIfItFails action instead = do
  action `catch` \(_e :: SomeException) -> do return instead

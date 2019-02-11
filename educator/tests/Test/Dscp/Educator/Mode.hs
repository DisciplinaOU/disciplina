{-# LANGUAGE OverloadedLabels #-}

module Test.Dscp.Educator.Mode
  ( module Test.Dscp.Educator.Mode
  , module Dscp.Core
  , module Dscp.Util
  , module Dscp.Util.Test
  , module Hspec
  , hash
  ) where

import Prelude hiding (fold)

import Control.Lens (makeLenses, (?~))
import GHC.IO.Unsafe
import qualified Loot.Log as Log
import qualified Pdf.FromLatex as Pdf
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath.Posix ((</>))
import qualified Test.Hspec as Hspec
import Test.QuickCheck (ioProperty, resize)
import Test.QuickCheck.Monadic (PropertyM, monadic, stop)

import Dscp.Config.Util
import Dscp.Core
import Dscp.Crypto
import Dscp.DB.CanProvideDB as DB
import Dscp.DB.CanProvideDB.Pure as PureDB
import Dscp.DB.SQL
import Dscp.Educator.Arbitrary ()
import Dscp.Educator.Config
import Dscp.Educator.Launcher
import Dscp.Educator.TestConfig
import Dscp.Resource.Keys
import Dscp.Rio
import Dscp.Util
import Dscp.Util.HasLens
import Dscp.Util.Test
import Dscp.Witness

import Test.Dscp.DB.SQL.Mode

type Trololo m = (MonadThrow m, MonadCatch m)

data TestEducatorCtx = TestEducatorCtx
    { _tecEducatorDb       :: SQL
    , _tecWitnessDb        :: DB.Plugin
    , _tecKeys             :: KeyResources EducatorNode
    , _tecPdfResourcePath  :: Pdf.ResourcePath
    , _tecWitnessKeys      :: KeyResources WitnessNode
    , _tecWitnessVariables :: TestWitnessVariables
    , _tecLogging          :: Log.Logging IO
    }
makeLenses ''TestEducatorCtx
deriveHasLensDirect ''TestEducatorCtx

deriveHasLens 'tecWitnessVariables ''TestEducatorCtx ''WitnessVariables
deriveHasLens 'tecWitnessVariables ''TestEducatorCtx ''TestWitnessVariables

type TestEducatorM = RIO TestEducatorCtx

instance MonadFail TestEducatorM where
    fail = error . toText

resourcePathVarName :: String
resourcePathVarName = "PDF_RESOURCE_PATH"

testResourcePath :: Pdf.ResourcePath
testResourcePath = unsafePerformIO $ do
    mpath <- lookupEnv resourcePathVarName
    path <- case mpath of
        Nothing -> do
            cd <- getCurrentDirectory
            let path = "../pdfs/template"
            putTextLn $ "No env variable " <> show resourcePathVarName <> " set, \
                        \assuming that PDF templates are in " <> show (cd </> path)
            return path
        Just path -> pure path
    return (Pdf.ResourcePath path)

runTestSqlM :: PostgresTestServer -> TestEducatorM a -> IO a
runTestSqlM testDb action =
    withEducatorConfig testEducatorConfig $
    withWitnessConfig (rcast testEducatorConfig) $
    withPostgresDb testDb $ \rollbackInEnd db ->
    runRIO testLogging $ do
        _tecWitnessKeys <- mkCommitteeStore committeeKeyParams
        _tecWitnessDb <- PureDB.plugin <$> liftIO PureDB.newCtxVar
        _tecWitnessVariables <- mkTestWitnessVariables (_tecWitnessKeys ^. krPublicKey)
                                                       _tecWitnessDb
        let _tecKeys = KeyResources $ mkSecretKeyData testSomeGenesisSecret
        let _tecEducatorDb = db
        let _tecLogging = testLogging
        let _tecPdfResourcePath = testResourcePath
        let ctx = TestEducatorCtx{..}
        runRIO ctx $ markWithinWriteSDLockUnsafe applyGenesisBlock

        liftIO . rollbackInEnd $ runRIO ctx action
  where
    committeeKeyParams :: CommitteeParamsRec
    committeeKeyParams = finaliseDeferredUnsafe $ mempty
        & tree #params . selection ?~ "open"
        & tree #params . option #participantN ?~ 0

educatorPropertyM
    :: Testable prop
    => (HasEducatorConfig => PropertyM TestEducatorM prop)
    -> PostgresTestServer
    -> Property
educatorPropertyM action testDb =
    monadic (ioProperty . runTestSqlM testDb)
            (withEducatorConfig testEducatorConfig $ void $ action >>= stop)

educatorProperty
    :: (Testable prop, Show a, Arbitrary a)
    => (HasEducatorConfig => a -> TestEducatorM prop)
    -> PostgresTestServer
    -> Property
educatorProperty action =
    educatorPropertyM $ pick (resize 5 arbitrary) >>= lift . action

sqlProperty
    :: (Testable prop, Show a, Arbitrary a)
    => (a -> DBT t TestEducatorM prop) -> PostgresTestServer -> Property
sqlProperty action =
    educatorProperty (invokeUnsafe . action)

sqlPropertyM
    :: Testable prop
    => PropertyM (DBT t TestEducatorM) prop -> PostgresTestServer -> Property
sqlPropertyM action testDb =
    monadic (ioProperty . runTestSqlM testDb . invokeUnsafe) (void $ action >>= stop)

orIfItFails :: MonadCatch m => m a -> a -> m a
orIfItFails action instead = do
  action `catch` \(_e :: SomeException) -> do return instead

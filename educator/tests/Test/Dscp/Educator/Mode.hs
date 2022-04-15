{-# LANGUAGE OverloadedLabels #-}

module Test.Dscp.Educator.Mode
  ( module Test.Dscp.Educator.Mode
  , module Dscp.Core
  , module Dscp.Util
  , module Dscp.Util.Test
  , module Hspec
  , hash
  ) where

import Universum hiding (fold)

import qualified Data.List as L
import Control.Lens (makeLenses)
import GHC.IO.Unsafe
import qualified Loot.Log as Log
import qualified Pdf.FromLatex as Pdf
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath.Posix ((</>))
import qualified Test.Hspec as Hspec
import Test.QuickCheck (resize)

import Dscp.Config.Util
import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQL
import Dscp.Educator.Config
import Dscp.Educator.Launcher
import Dscp.Educator.TestConfig
import Dscp.Resource.AppDir
import Dscp.Rio
import Dscp.Util
import Dscp.Util.HasLens
import Dscp.Util.Rethrow
import Dscp.Util.Test

import Test.Dscp.DB.SQL.Mode

type Trololo m = (MonadThrow m, MonadCatch m)

data TestEducatorCtx = TestEducatorCtx
    { _tecEducatorDb       :: SQL
    , _tecPubAddress       :: PubAddress
    , _tecLanguage         :: Language
    , _tecPdfLatexPath     :: Pdf.LatexPath
    , _tecPdfResourcePath  :: Pdf.ResourcePath
    , _tecDownloadBaseUrl  :: Pdf.DownloadBaseUrl
    , _tecLogging          :: Log.Logging IO
    , _tecAppDir           :: AppDir
    , _tecIssuerInfo       :: CertificateIssuerResource
    }

makeLenses ''TestEducatorCtx
deriveHasLensDirect ''TestEducatorCtx

type TestEducatorM = RIO TestEducatorCtx

instance MonadFail TestEducatorM where
    fail = error . toText

testGenesisSecrets :: [SecretKey]
testGenesisSecrets = detGen 123 $ vectorUnique 10

testSomeGenesisSecret :: SecretKey
testSomeGenesisSecret = L.head testGenesisSecrets

testPubAddress :: PubAddress
testPubAddress = detGen 123 arbitrary

resourcePathVarName :: String
resourcePathVarName = "PDF_RESOURCE_PATH"

testLatexPath :: Pdf.LatexPath
testLatexPath = Pdf.LatexPath "xelatex"

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

testDownloadBaseUrl :: Pdf.DownloadBaseUrl
testDownloadBaseUrl = Pdf.DownloadBaseUrl $
    nothingToPanic "url is correct" $
    parseBaseUrl "https://educator.disciplina.io/api/certificates/v1/cert"

runTestSqlM :: PostgresTestServer -> TestEducatorM a -> IO a
runTestSqlM testDb action =
    withEducatorConfig testEducatorConfig $
    withPostgresDb testDb $ \rollbackInEnd db ->
    runRIO testLogging $ do
        let _tecPubAddress = testPubAddress
        let _tecEducatorDb = db
        let _tecLogging = testLogging
        let _tecLanguage = EN
        let _tecPdfLatexPath = testLatexPath
        let _tecPdfResourcePath = testResourcePath
        let _tecDownloadBaseUrl = testDownloadBaseUrl
        let _tecAppDir = error "AppDir is not defined"
        let _tecIssuerInfo = KnownIssuerInfo certificateIssuerInfoEx
        let ctx = TestEducatorCtx{..}

        liftIO . rollbackInEnd $ runRIO ctx action

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
    educatorProperty (throwsOnlyExpected . invokeUnsafe . action)

sqlPropertyM
    :: Testable prop
    => PropertyM (DBT t TestEducatorM) prop -> PostgresTestServer -> Property
sqlPropertyM action testDb =
    monadic (ioProperty . runTestSqlM testDb . throwsOnlyExpected . invokeUnsafe)
            (void $ action >>= stop)

orIfItFails :: MonadCatch m => m a -> a -> m a
orIfItFails action instead = do
  action `catch` \(_e :: SomeException) -> do return instead

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData       #-}

module Test.Dscp.Educator.Mode
  ( module Test.Dscp.Educator.Mode
  , module Dscp.Core
  , module Dscp.Util
  , module Dscp.Util.Test
  , module Test.Hspec
  , hash
  ) where

import Prelude hiding (fold)

import Control.Lens (makeLenses, (&~), (.=), (?=))
import Data.Coerce (coerce)
import Data.Default (def)
import Loot.Config.Record (finaliseDeferredUnsafe, option, sub)
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
import Dscp.Educator.Launcher
import Dscp.Resource.Keys
import Dscp.Resource.SQLite
import Dscp.Rio
import Dscp.Snowdrop.Actions
import Dscp.Util
import Dscp.Util.HasLens
import Dscp.Util.Test
import Dscp.Witness

type Trololo m = (MonadThrow m, MonadCatch m)

data TestEducatorCtx = TestEducatorCtx
    { _tecEducatorDb       :: SQLiteDB
    , _tecWitnessDb        :: DB.Plugin
    , _tecKeys             :: KeyResources EducatorNode
    , _tecWitnessKeys      :: KeyResources WitnessNode
    , _tecWitnessVariables :: WitnessVariables
    , _tecLogging          :: Log.Logging IO
    }
makeLenses ''TestEducatorCtx
deriveHasLensDirect ''TestEducatorCtx

deriveHasLens 'tecWitnessVariables ''TestEducatorCtx ''WitnessVariables

type TestEducatorM = RIO TestEducatorCtx

testCommittee :: Committee
testCommittee =
    CommitteeOpen
    { commN = 2
    , commSecret = detGen 121 ((leftToPanic . mkCommitteeSecret) <$> arbitrary)
    }

-- | Witness test configuration.
-- Only those parts are defined which are actually used in tests.
--
-- Hello copy-pasta!
testEducatorConfig :: EducatorConfigRec
testEducatorConfig =
    finaliseDeferredUnsafe $ def &~ do
        sub #core .= def &: do
            sub #generated . option #genesisInfo ?= formGenesisInfo genConfig
            option #genesis ?= genConfig
            option #fee ?= feeCoefs
            option #slotDuration ?= 10000000
  where
    genConfig =
        GenesisConfig
        { gcGenesisSeed = "meme tests"
        , gcGovernance = GovCommittee testCommittee
        , gcDistribution = GenesisDistribution . one $ GDEqual (Coin 100)
        }
    feeCoefs =
        FeeConfig
        { fcMoney = LinearFeePolicy
            FeeCoefficients
            { fcMinimal       = Coin 10
            , fcMultiplier    = 0.1
            }
        , fcPublication = LinearFeePolicy
            FeeCoefficients
            { fcMinimal       = Coin 10
            , fcMultiplier    = 0.1
            }
        }

-- TODO: remove this cope-paste when DSCP-335 is merged, or in DSCP-335 itself
mkTestWitnessVariables
    :: (MonadIO m, HasWitnessConfig)
    => PublicKey -> DB.Plugin -> m WitnessVariables
mkTestWitnessVariables issuer db = do
    _wvMempool    <- newMempoolVar issuer
    _wvSDActions  <- liftIO $ runReaderT initSDActions db
    _wvRelayState <- newRelayState
    _wvSDLock     <- newSDLock
    return WitnessVariables{..}

runTestSQLiteM :: TestEducatorM a -> IO a
runTestSQLiteM action =
    withEducatorConfig testEducatorConfig $
    withWitnessConfig (rcast testEducatorConfig) $
    runRIO _tecLogging $ do
        _tecKeys <- genStore (Just $ CommitteeParamsOpen 0)
        _tecWitnessDb <- PureDB.plugin <$> liftIO PureDB.newCtxVar
        _tecWitnessVariables <- mkTestWitnessVariables (_tecKeys ^. krPublicKey)
                                                       _tecWitnessDb
        let _tecWitnessKeys = coerce _tecKeys
        bracket openDB closeSQLiteDB $ \_tecEducatorDb -> do
            let ctx = TestEducatorCtx{..}
            runRIO ctx $ do
                  markWithinWriteSDLockUnsafe applyGenesisBlock
                  action
  where
    dbParams = SQLiteParams SQLiteInMemory
    openDB = do
        db <- openSQLiteDB dbParams
        prepareEducatorSchema db
        return db
    _tecLogging = testLogging

educatorPropertyM
    :: Testable prop
    => (HasEducatorConfig => PropertyM TestEducatorM prop)
    -> Property
educatorPropertyM action =
    monadic (ioProperty . runTestSQLiteM)
            (withEducatorConfig testEducatorConfig $ void $ action >>= stop)

educatorProperty
    :: (Testable prop, Show a, Arbitrary a)
    => (HasEducatorConfig => a -> TestEducatorM prop)
    -> Property
educatorProperty action =
    educatorPropertyM $ pick (resize 5 arbitrary) >>= lift . action

sqliteProperty
    :: (Testable prop, Show a, Arbitrary a)
    => (a -> DBT t w TestEducatorM prop) -> Property
sqliteProperty action =
    educatorProperty (invokeUnsafe . action)

sqlitePropertyM :: Testable prop => PropertyM (DBT t w TestEducatorM) prop -> Property
sqlitePropertyM action =
    monadic (ioProperty . runTestSQLiteM . invokeUnsafe) (void $ action >>= stop)

orIfItFails :: MonadCatch m => m a -> a -> m a
orIfItFails action instead = do
  action `catch` \(_e :: SomeException) -> do return instead

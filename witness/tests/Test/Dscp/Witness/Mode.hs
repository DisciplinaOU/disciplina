{-# LANGUAGE CPP              #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}

module Test.Dscp.Witness.Mode
    ( WitnessTestMode'
    , witnessProperty

    , testGenesisSecrets
    , testSomeGenesisSecret
    , testGenesisAddresses
    , testGenesisAddressAmount
    , testCommittee
    , testCommitteeSecrets
    , testCommitteeAddrs
    ) where

import Control.Lens (makeLenses, (&~), (.=), (?=))
import Data.Default (def)
import qualified Data.List as L
import qualified Data.Map as M
import Loot.Config.Record (finaliseDeferredUnsafe, option, sub)
import Loot.Log (Logging (..))

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.CanProvideDB as DB
import Dscp.DB.CanProvideDB.Pure as PureDB
import Dscp.Resource.Keys
import Dscp.Rio
import Dscp.Snowdrop.Actions
import Dscp.Util
import Dscp.Util.HasLens
import Dscp.Util.Test
import Dscp.Witness

----------------------------------------------------------------------------
-- Test witness mode
----------------------------------------------------------------------------

data TestWitnessCtx = TestWitnessCtx
    { _twcVars    :: WitnessVariables
    , _twcLogging :: Logging IO
    , _twcKeys    :: KeyResources WitnessNode
    , _twcDb      :: DB.Plugin
    }

makeLenses ''TestWitnessCtx
deriveHasLensDirect ''TestWitnessCtx

deriveHasLens 'twcVars ''TestWitnessCtx ''WitnessVariables

type WitnessTestMode' = RIO TestWitnessCtx

----------------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------------

testGenesisSecrets :: [SecretKey]
testGenesisSecrets = detGen 123 $ vectorUnique 10

testSomeGenesisSecret :: SecretKey
testSomeGenesisSecret = L.head testGenesisSecrets

testGenesisAddresses :: [Address]
testGenesisAddresses = mkAddr . toPublic <$> testGenesisSecrets

testGenesisAddressAmount :: Coin
testGenesisAddressAmount = Coin 10000

testCommittee :: Committee
testCommittee =
    CommitteeOpen
    { commN = 2
    , commSecret = detGen 121 ((leftToPanic . mkCommitteeSecret) <$> arbitrary)
    }

testCommitteeSecrets :: [SecretKey]
testCommitteeSecrets = openCommitteeSecrets testCommittee

testCommitteeAddrs :: [Address]
testCommitteeAddrs = map (mkAddr . toPublic) testCommitteeSecrets

-- | Witness test configuration.
-- Only those parts are defined which are actually used in tests.
testWitnessConfig :: WitnessConfigRec
testWitnessConfig =
    finaliseDeferredUnsafe $ def &~ do
        sub #core .= def &: do
            sub #generated . option #genesisInfo ?= formGenesisInfo genConfig
            option #genesis ?= genConfig
            option #fee ?= feeCoefs
            option #slotDuration ?= 10000000
  where
    genesisAddressMap =
        GenAddressMap $ M.fromList $
        map (, testGenesisAddressAmount) testGenesisAddresses
    genConfig =
        GenesisConfig
        { gcGenesisSeed = "meme tests"
        , gcGovernance = GovCommittee testCommittee
        , gcDistribution = GenesisDistribution
            [ GDEqual testGenesisAddressAmount
            , GDSpecific genesisAddressMap
            ]
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

----------------------------------------------------------------------------
-- Runner
----------------------------------------------------------------------------

mkTestWitnessVariables
    :: (MonadIO m, HasWitnessConfig)
    => PublicKey -> DB.Plugin -> m WitnessVariables
mkTestWitnessVariables issuer db = do
    _wvMempool    <- newMempoolVar issuer
    _wvSDActions  <- liftIO $ runReaderT initSDActions db
    _wvRelayState <- newRelayState
    _wvSDLock     <- newSDLock
    return WitnessVariables{..}

runWitnessTestMode :: WitnessTestMode' a -> IO a
runWitnessTestMode action =
    withWitnessConfig testWitnessConfig $ runRIO testLogging $ do
        _twcKeys <- genStore (Just $ CommitteeParamsOpen 0)
        _twcDb   <- PureDB.plugin <$> liftIO PureDB.newCtxVar
        _twcVars <- mkTestWitnessVariables (_twcKeys ^. krPublicKey) _twcDb
        let _twcLogging = testLogging
        let ctx = TestWitnessCtx{..}

        runRIO ctx $ do
            markWithinWriteSDLockUnsafe applyGenesisBlock
            action

witnessProperty
    :: Testable prop
    => ((HasWitnessConfig, WithinWriteSDLock) => PropertyM WitnessTestMode' prop)
    -> Property
witnessProperty action =
    -- Note on 'execUnmasked': for some reason, tests are run under 'mask'.
    -- Snowdrop expects it to be not like that, for instance `mappend` on
    -- snowdrop actions, which is used in validation, uses 'concurrently' under
    -- hood, which may hang when is executed under 'mask'.
    monadic (ioProperty . execUnmasked . runWitnessTestMode) $ do
        prop <- markWithinWriteSDLockUnsafe $
                withWitnessConfig testWitnessConfig action
        void $ stop prop

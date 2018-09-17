{-# LANGUAGE CPP              #-}
{-# LANGUAGE OverloadedLabels #-}

module Test.Dscp.Witness.Mode
    ( WitnessTestMode
    , witnessProperty

    , testGenesisSecrets
    , testGenesisAddresses
    , testGenesisAddressAmount
    , testCommittee
    ) where

import Control.Lens (makeLenses, (&~), (?=))
import Data.Default (def)
import qualified Data.Map as M
import Fmt ((+|), (+||), (|+), (||+))
import Loot.Base.HasLens (HasLens (..))
import Loot.Config.Record (finaliseDeferedUnsafe, option, sub)
import Loot.Log (Level (Warning), Logging (..))

import Dscp.Core
import Dscp.Crypto
import Dscp.Resource.Keys
import Dscp.Rio
import Dscp.Snowdrop
import Dscp.Util
import Dscp.Util.Test
import Dscp.Web.Metrics
import Dscp.Witness

----------------------------------------------------------------------------
-- Test witness mode
----------------------------------------------------------------------------

data TestWitnessCtx = TestWitnessCtx
    { _twcMempoolVar :: MempoolVar
    , _twcSDVars     :: SDVars
    , _twcSDLock     :: SDLock
    , _twcLogging    :: Logging IO
    , _twcKeys       :: KeyResources WitnessNode
    , _twcRelayState :: RelayState
    }

makeLenses ''TestWitnessCtx

#define GenHasLens(SUBRES, IMPL) \
    instance HasLens (SUBRES) TestWitnessCtx (SUBRES) where \
        lensOf = IMPL

GenHasLens(MempoolVar, twcMempoolVar)
GenHasLens(SDVars, twcSDVars)
GenHasLens(SDLock, twcSDLock)
GenHasLens(Logging IO, twcLogging)
GenHasLens(KeyResources WitnessNode, twcKeys)
GenHasLens(Maybe WitnessWebParams, seeOnly Nothing)
GenHasLens(RelayState, twcRelayState)
GenHasLens(MetricsEndpoint, seeOnly (MetricsEndpoint Nothing))

type WitnessTestMode = RIO TestWitnessCtx

----------------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------------

testGenesisSecrets :: [SecretKey]
testGenesisSecrets = detGen 123 $ vectorUnique 10

testGenesisAddresses :: [Address]
testGenesisAddresses = mkAddr . toPublic <$> testGenesisSecrets

testGenesisAddressAmount :: Coin
testGenesisAddressAmount = Coin 10000

testCommittee :: Committee
testCommittee =
    CommitteeOpen
    { commN = 2
    , commSecret = detGen 12 (CommitteeSecret <$> arbitrary)
    }

-- | Witness test configuration.
-- Only those parts are defined which are actually used in tests.
testWitnessConfig :: WitnessConfigRec
testWitnessConfig =
    finaliseDeferedUnsafe $ def &~ do
        sub #core . sub #generated . option #genesisInfo ?= formGenesisInfo genConfig
        sub #core . option #genesis ?= genConfig
        sub #core . option #fee ?= feeCoefs
  where
    genesisAddressMap =
        GenAddressMap $ M.fromList $
        map (, testGenesisAddressAmount) testGenesisAddresses
    genConfig =
        GenesisConfig
        { gcGenesisSeed = "meme tests"
        , gcGovernance = GovCommittee testCommittee
        , gcDistribution = GenesisDistribution . one $ GDSpecific genesisAddressMap
        }
    feeCoefs =
        FeeCoefficients
        { fcMinimal       = Coin 10
        , fcMultiplier    = 0.1
        , fcMinimalPub    = Coin 10
        , fcMultiplierPub = 0.0001
        }

----------------------------------------------------------------------------
-- Runner
----------------------------------------------------------------------------

runWitnessTestMode :: WitnessTestMode a -> IO a
runWitnessTestMode action =
    withWitnessConfig testWitnessConfig $ runRIO _twcLogging $ do
        _twcSDVars <- initSDActions
        _twcSDLock <- newSDLock
        _twcRelayState <- newRelayState
        _twcKeys <- genStore (Just $ CommitteeParamsOpen 0)
        _twcMempoolVar <- newMempoolVar (_krPublicKey _twcKeys)
        let ctx = TestWitnessCtx{..}
        runRIO ctx $ do
            markWithinWriteSDLockUnsafe applyGenesisBlock
            action
  where
    _twcLogging = Logging
        { _log = \lvl _ txt ->
            when (lvl >= Warning) $
                putTextLn $ "[" +|| lvl ||+ "] " +| txt |+ ""
        , _logName = pure (error "No logging name in tests")
        }

witnessProperty
    :: Testable prop
    => ((HasWitnessConfig, WithinWriteSDLock) => PropertyM WitnessTestMode prop)
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

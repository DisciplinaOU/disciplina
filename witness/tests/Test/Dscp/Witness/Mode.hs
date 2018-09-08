{-# LANGUAGE CPP              #-}
{-# LANGUAGE OverloadedLabels #-}

module Test.Dscp.Witness.Mode
    ( WitnessTestMode
    , witnessProperty

    , testGenesisSecrets
    , testGenesisAddresses
    , testGenesisAddressAmount
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

-- | Witness test configuration.
-- Only those parts are defined which are actually used in tests.
testWitnessConfig :: WitnessConfigRec
testWitnessConfig =
    finaliseDeferedUnsafe $ def &~ do
        sub #core . sub #generated . option #genesisInfo ?= formGenesisInfo genConfig
        sub #core . option #genesis ?= genConfig
  where
    genesisAddressMap =
        GenAddressMap $ M.fromList $
        map (, testGenesisAddressAmount) testGenesisAddresses
    genConfig =
        GenesisConfig
        { gcGenesisSeed = "meme tests"
        , gcGovernance = error "Committee is not provided"
        , gcDistribution = GenesisDistribution . one $ GDSpecific genesisAddressMap
        }

----------------------------------------------------------------------------
-- Runner
----------------------------------------------------------------------------

runWitnessTestMode :: WitnessTestMode a -> IO a
runWitnessTestMode action =
    withWitnessConfig testWitnessConfig $ do
        _twcMempoolVar <- newMempoolVar (_krPublicKey _twcKeys)
        _twcSDVars <- runRIO _twcLogging initSDActions
        _twcSDLock <- newSDLock
        _twcRelayState <- newRelayState
        let ctx = TestWitnessCtx{..}
        runRIO ctx $ do
            markWithinWriteSDLockUnsafe applyGenesisBlock
            action
  where
    _twcKeys =
        let sk = detGen 22 arbitrary
        in KeyResources
           { _krSecretKey = sk
           , _krPublicKey = toPublic sk
           }
    _twcLogging = Logging
        { _log = \lvl _ txt ->
            when (lvl >= Warning) $
                putTextLn $ "[" +|| lvl ||+ "] " +| txt |+ ""
        , _logName = pure (error "No logging name in tests")
        }

witnessProperty
    :: Testable prop
    => (WithinWriteSDLock => PropertyM WitnessTestMode prop)
    -> Property
witnessProperty action =
    monadic (ioProperty . runWitnessTestMode)
            (markWithinWriteSDLockUnsafe action >>= void . stop)

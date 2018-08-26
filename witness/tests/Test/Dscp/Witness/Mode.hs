{-# LANGUAGE CPP              #-}
{-# LANGUAGE OverloadedLabels #-}

module Test.Dscp.Witness.Mode
    ( WitnessTestMode
    , witnessProperty_
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
import Loot.Log (Level (Error, Warning), Logging (..))

import Dscp.Core
import Dscp.Crypto
import Dscp.Rio
import Dscp.Snowdrop
import Dscp.Util.Test
import Dscp.Witness

----------------------------------------------------------------------------
-- Test witness mode
----------------------------------------------------------------------------

data TestWitnessCtx = TestWitnessCtx
    { _twcMempoolVar :: MempoolVar
    , _twcSDVars     :: SDVars
    , _twcSDLock     :: SDLock
    , _twcLogging    :: Logging IO
    }

makeLenses ''TestWitnessCtx

#define GenHasLens(SUBRES, IMPL) \
    instance HasLens (SUBRES) TestWitnessCtx (SUBRES) where \
        lensOf = IMPL

GenHasLens(MempoolVar, twcMempoolVar)
GenHasLens(SDVars, twcSDVars)
GenHasLens(SDLock, twcSDLock)
GenHasLens(Logging IO, twcLogging)

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
        sub #core . sub #generated . option #genesisInfo ?= genInfo
  where
    genInfo =
      GenesisInfo
      { giAddressMap =
          GenAddressMap $ M.fromList $
          map (, testGenesisAddressAmount) testGenesisAddresses
      , giGenesisBlock = error "Genesis block undefined"
      }

----------------------------------------------------------------------------
-- Runner
----------------------------------------------------------------------------

runWitnessTestMode :: WitnessTestMode a -> IO a
runWitnessTestMode action =
    withWitnessConfig testWitnessConfig $ do
        _twcMempoolVar <- newMempoolVar
        _twcSDVars <- runRIO _twcLogging initSDActions
        _twcSDLock <- newSDLock
        let ctx = TestWitnessCtx{..}
        runRIO ctx action
  where
    _twcLogging = Logging
        { _log = \lvl _ txt ->
            when (lvl == Warning || lvl == Error) $
                putTextLn $ "[" +|| lvl ||+ "] " +| txt |+ ""
        , _logName = pure (error "No logging name in tests")
        }

witnessProperty_
    :: (WithinWriteSDLock => PropertyM WitnessTestMode ())
    -> Property
witnessProperty_ action =
    monadic (ioProperty . runWitnessTestMode)
            (markWithinWriteSDLockUnsafe action)

witnessProperty
    :: Testable prop
    => (WithinWriteSDLock => PropertyM WitnessTestMode prop)
    -> Property
witnessProperty action = witnessProperty_ $ action >>= stop

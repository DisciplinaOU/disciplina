{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}

module Dscp.Witness.TestConfig
    ( testGenesisSecrets
    , testSomeGenesisSecret
    , testGenesisAddresses
    , testGenesisAddressAmount
    , testCommittee
    , testCommitteeSecrets
    , testCommitteeAddrs
    , testWitnessConfigP
    , testWitnessConfig
    , mkTestWitnessVariables
    ) where

import Control.Lens ((.=), (?=))
import Data.Default (def)
import qualified Data.List as L
import qualified Data.Map as M
import Dscp.DB.CanProvideDB as DB
import Loot.Config.Record (finaliseDeferredUnsafe, option, sub)

import Dscp.Core
import Dscp.Crypto
import Dscp.Snowdrop.Actions (initSDActions)
import Dscp.Util
import Dscp.Util.Test
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Context
import Dscp.Witness.Mempool (newMempoolVar)
import Dscp.Witness.Relay
import Dscp.Witness.SDLock

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
testWitnessConfigP :: WitnessConfigRecP
testWitnessConfigP = def &: do
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

testWitnessConfig :: WitnessConfigRec
testWitnessConfig = finaliseDeferredUnsafe testWitnessConfigP

mkTestWitnessVariables
    :: (MonadIO m, HasWitnessConfig)
    => PublicKey -> DB.Plugin -> m WitnessVariables
mkTestWitnessVariables issuer dbPlugin = do
    _wvMempool    <- newMempoolVar issuer
    _wvSDActions  <- liftIO $ runReaderT initSDActions dbPlugin
    _wvRelayState <- newRelayState
    _wvSDLock     <- newSDLock
    return WitnessVariables{..}

{-# LANGUAGE OverloadedLabels #-}

-- | Helpers for starting an Witness node

module Dscp.Witness.Launcher.Runner
    ( formWitnessContext
    , launchWitnessRealMode
    ) where

import Loot.Config (option, sub)
import Loot.Log (MonadLogging)

import Dscp.Resource.Class (AllocResource (..), InitParams (..))
import Dscp.Resource.Functions
import Dscp.Resource.Keys (krPublicKey)
import Dscp.Rio (runRIO)
import Dscp.Snowdrop.Actions (initSDActions)
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Mode (WitnessContext (..), WitnessRealMode)
import Dscp.Witness.Launcher.Resource (WitnessResources (..), wrKey)
import Dscp.Witness.Mempool (newMempoolVar)
import qualified Dscp.Witness.Relay as Relay
import qualified Dscp.Witness.SDLock as Lock

-- | Make up Witness context from dedicated pack of allocated resources.
formWitnessContext
    :: (MonadIO m, MonadCatch m, MonadLogging m, HasWitnessConfig)
    => WitnessResources
    -> m WitnessContext
formWitnessContext _wcResources = do
    _wcMempool    <- newMempoolVar $ _wcResources^.wrKey.krPublicKey
    _wcSDActions  <- initSDActions
    _wcRelayState <- Relay.newRelayState
    _wcSDLock     <- Lock.newSDLock
    pure $ WitnessContext
        { _wcResources
        , _wcMempool
        , _wcSDActions
        , _wcRelayState
        , _wcSDLock
        }

-- | Given params, allocate resources, construct node context and run
-- `WitnessWorkMode` monad. Any synchronous exceptions are handled inside.
launchWitnessRealMode
    :: WitnessConfigRec
    -> (HasWitnessConfig => WitnessRealMode ())
    -> IO ()
launchWitnessRealMode config action =
    exitSilentlyOnFailure $
    withWitnessConfig config $
    runResourceAllocation appDesc initParams (allocResource ()) $
        \resources -> do
            ctx <- formWitnessContext resources
            runRIO ctx action
  where
    appDesc = "Witness (real mode)"
    initParams = InitParams
        { ipLoggingParams = config ^. sub #witness . option #logging
        }

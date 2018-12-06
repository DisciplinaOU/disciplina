{-# LANGUAGE OverloadedLabels #-}

-- | Helpers for starting an Witness node

module Dscp.Witness.Launcher.Runner
    ( mkWitnessVariables
    , formWitnessContext
    , launchWitnessRealMode
    ) where

import Loot.Log (MonadLogging)

import Dscp.Config (sub)
import Dscp.Resource.Class (AllocResource (..), InitParams (..))
import Dscp.Resource.Functions
import Dscp.Resource.Keys (krPublicKey)
import Dscp.Rio (runRIO)
import Dscp.Snowdrop.Actions (initSDActions)
import Dscp.Util.Time
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Context
import Dscp.Witness.Launcher.Resource
import Dscp.Witness.Mempool (newMempoolVar)
import qualified Dscp.Witness.Relay as Relay
import qualified Dscp.Witness.SDLock as Lock

mkWitnessVariables
    :: (MonadIO m, MonadCatch m, MonadLogging m, HasWitnessConfig)
    => WitnessResources -> m WitnessVariables
mkWitnessVariables resources = do
    _wvMempool    <- newMempoolVar (resources^.wrKey.krPublicKey)
    _wvSDActions  <- liftIO $ runReaderT initSDActions (_wrDB resources)
    _wvRelayState <- Relay.newRelayState
    _wvSDLock     <- Lock.newSDLock
    let _wvTime = realTimeActions
    return WitnessVariables{..}

-- | Make up Witness context from dedicated pack of allocated resources.
formWitnessContext
    :: (MonadIO m, MonadCatch m, MonadLogging m, HasWitnessConfig)
    => WitnessResources
    -> m WitnessContext
formWitnessContext _wcResources = do
    _wcVars <- mkWitnessVariables _wcResources
    pure $ WitnessContext
        { _wcResources
        , _wcVars
        }

-- | Given params, allocate resources, construct node context and run
-- `WitnessWorkMode` monad. Any synchronous exceptions are handled inside.
launchWitnessRealMode
    :: WitnessConfigRec
    -> (HasWitnessConfig => WitnessRealMode ())
    -> IO ()
launchWitnessRealMode config action =
    exitSilentlyOnFailure $
    runResourceAllocation appDesc initParams (allocResource config) $
        \resources -> withWitnessConfig config $ do
            ctx <- formWitnessContext resources
            runRIO ctx action
  where
    appDesc = "Witness (real mode)"
    initParams = InitParams
        { ipLoggingParams = config ^. sub #witness . sub #logging
        }

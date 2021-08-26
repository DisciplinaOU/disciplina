{-# LANGUAGE OverloadedLabels #-}

-- | Helpers for starting an Faucet node

module Dscp.Faucet.Launcher.Runner
    ( formFaucetContext
    , launchFaucetRealMode
    ) where

import Dscp.Config
import Dscp.Faucet.Config
import Dscp.Faucet.Launcher.Mode
import Dscp.Faucet.Launcher.Resource
import Dscp.Faucet.Variables
import Dscp.Resource.Class (AllocResource (..), InitParams (..))
import Dscp.Resource.Functions
import Dscp.Rio (runRIO)

-- | Make up Faucet context from dedicated pack of allocated resources.
formFaucetContext
    :: MonadIO m
    => FaucetResources -> m FaucetContext
formFaucetContext _fcResources = do
    _fcVariables <- mkFaucetVariables
    pure $ FaucetContext {..}

-- | Given params, allocate resources, construct node context and run
-- `FaucetWorkMode` monad. Any synchronous exceptions are handled inside.
launchFaucetRealMode
    :: FaucetConfigRec
    -> (HasFaucetConfig => FaucetRealMode Void)
    -> IO a
launchFaucetRealMode config action =
    exitSilentlyOnFailure $
    runResourceAllocation appDesc initParams (allocResource config) $
        \resources -> withFaucetConfig config $ do
            ctx <- formFaucetContext resources
            runRIO ctx action
  where
    appDesc = "Faucet (real mode)"
    initParams = InitParams
        { ipLoggingParams = config ^. sub #faucet . sub #logging
        }

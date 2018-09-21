-- | Helpers for starting an Faucet node

module Dscp.Faucet.Launcher.Runner
    ( formFaucetContext
    , launchFaucetRealMode
    ) where

import Dscp.Faucet.Config
import Dscp.Faucet.Launcher.Mode
import Dscp.Faucet.Launcher.Params
import Dscp.Faucet.Launcher.Resource
import Dscp.Faucet.Variables
import Dscp.Rio (runRIO)
import Dscp.Resource.Class (AllocResource (..), InitParams (..))
import Dscp.Resource.Functions

-- | Make up Faucet context from dedicated pack of allocated resources.
formFaucetContext
    :: MonadIO m
    => FaucetParams -> FaucetResources -> m FaucetContext
formFaucetContext _fcParams _fcResources = do
    _fcVariables <- mkFaucetVariables
    pure $ FaucetContext {..}

-- | Given params, allocate resources, construct node context and run
-- `FaucetWorkMode` monad. Any synchronous exceptions are handled inside.
launchFaucetRealMode
    :: FaucetConfigRec
    -> FaucetParams
    -> (HasFaucetConfig => FaucetRealMode ())
    -> IO ()
launchFaucetRealMode config params@FaucetParams{..} action =
    exitSilentlyOnFailure $
    withFaucetConfig config $
    runResourceAllocation appDesc initParams (allocResource params) $
        \resources -> do
            ctx <- formFaucetContext params resources
            runRIO ctx action
  where
    appDesc = "Faucet (real mode)"
    initParams = InitParams{ ipLoggingParams = _fpLoggingParams }

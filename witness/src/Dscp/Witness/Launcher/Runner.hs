-- | Helpers for starting an Witness node

module Dscp.Witness.Launcher.Runner
    ( formWitnessContext
    , launchWitnessRealMode
    ) where

import Dscp.Launcher.Rio (runRIO)
import Dscp.Resource.Class (AllocResource (..), InitParams (..))
import Dscp.Resource.Functions
import Dscp.Snowdrop.Actions (initSDActions)
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Mode (WitnessContext (..), WitnessRealMode)
import Dscp.Witness.Launcher.Params (WitnessParams (..))
import Dscp.Witness.Launcher.Resource (WitnessResources (..))
import Dscp.Witness.Mempool (newMempoolVar)

-- | Make up Witness context from dedicated pack of allocated resources.
formWitnessContext :: WitnessParams -> WitnessResources -> IO WitnessContext
formWitnessContext _wcParams _wcResources = do
    _wcMempool <- newMempoolVar
    _wcSDActions <- initSDActions
    pure $ WitnessContext {..}

-- | Given params, allocate resources, construct node context and run
-- `WitnessWorkMode` monad. Any synchronous exceptions are handled inside.
launchWitnessRealMode
    :: WitnessConfigRec
    -> WitnessParams
    -> (HasWitnessConfig => WitnessRealMode ())
    -> IO ()
launchWitnessRealMode config params@WitnessParams{..} action =
    exitSilentlyOnFailure $
    withWitnessConfig config $
    runResourceAllocation appDesc initParams (allocResource params) $
        \resources -> do
            ctx <- formWitnessContext params resources
            runRIO ctx action
  where
    appDesc = "Witness (real mode)"
    initParams = InitParams{ ipLoggingParams = wpLoggingParams }

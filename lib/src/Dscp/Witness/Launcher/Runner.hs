-- | Helpers for starting an Witness node

module Dscp.Witness.Launcher.Runner where

import Dscp.Launcher.Rio (runRIO)
import Dscp.Resource (AllocResource (..), InitParams (..), runResourceAllocation)
import Dscp.Snowdrop.Actions (initSDActions)
import Dscp.Witness.Launcher.Mode (WitnessContext (..), WitnessRealMode)
import Dscp.Witness.Launcher.Params (WitnessParams (..))
import Dscp.Witness.Launcher.Resource (WitnessResources (..))
import Dscp.Witness.Mempool (newMempoolVar)

-- | Make up Witness context from dedicated pack of allocated resources.
formWitnessContext :: WitnessResources -> IO WitnessContext
formWitnessContext res@WitnessResources{..} = do
    _wcMempool <- newMempoolVar
    _wcSDActions <- initSDActions
    let _wcResources = res
    pure $ WitnessContext {..}

runWitnessRealMode :: WitnessContext -> WitnessRealMode () -> IO ()
runWitnessRealMode = runRIO

-- | Given params, allocate resources, construct node context and run
-- `WitnessWorkMode` monad. Any synchronous exceptions are handled inside.
launchWitnessRealMode :: WitnessParams -> WitnessRealMode () -> IO ()
launchWitnessRealMode params@WitnessParams{..} action =
    void $
    runResourceAllocation appDesc initParams (allocResource params) $
        \resources -> do
            ctx <- formWitnessContext resources
            runWitnessRealMode ctx action
  where
    appDesc = "Witness (real mode)"
    initParams = InitParams{ ipLoggingParams = wpLoggingParams }

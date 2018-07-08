-- | Helpers for starting an Witness node

module Dscp.Witness.Launcher.Runner where

import Dscp.Launcher.Rio (runRIO)
import Dscp.Resource (AllocResource (..), InitParams (..), runResourceAllocation)
import Dscp.Witness.Launcher.Mode (WitnessContext (..), WitnessRealMode)
import Dscp.Witness.Launcher.Params (WitnessParams (..))
import Dscp.Witness.Launcher.Resource (WitnessResources (..))

-- TODO Maybe this function should be "-> IO WitnessContext" and other
-- non-resource context parts can be allocated here.
-- | Make up Witness context from dedicated pack of allocated resources.
formWitnessContext :: WitnessResources -> WitnessContext
formWitnessContext res@WitnessResources{..} =
    WitnessContext { _wcResources = res }

runWitnessRealMode :: WitnessContext -> WitnessRealMode () -> IO ()
runWitnessRealMode = runRIO

-- | Given params, allocate resources, construct node context and run
-- `WitnessWorkMode` monad. Any synchronous exceptions are handled inside.
launchWitnessRealMode :: WitnessParams -> WitnessRealMode () -> IO ()
launchWitnessRealMode params@WitnessParams{..} action =
    void $
    runResourceAllocation appDesc initParams (allocResource params) $
      \resources ->
        let ctx = formWitnessContext resources
        in runWitnessRealMode ctx action
  where
    appDesc = "Witness (real mode)"
    initParams = InitParams{ ipLoggingParams = wpLoggingParams }

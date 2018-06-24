-- | Helpers for starting an Witness node

module Dscp.Witness.Launcher.Runner where

import Universum

import Control.Monad.Component (runComponentM)

import Dscp.Launcher.Rio (runRIO)
import Dscp.Resource (AllocResource (..))
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
-- `WitnessWorkMode` monad.
launchWitnessRealMode ::WitnessParams -> WitnessRealMode () -> IO ()
launchWitnessRealMode params action =
    runComponentM "Witness (real mode)" (allocResource params) $
      \resources ->
        let ctx = formWitnessContext resources
        in            runWitnessRealMode ctx action

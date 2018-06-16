-- | Helpers for starting an Witness node

module Dscp.Witness.Launcher.Runner where

import Universum

import Control.Monad.Component (runComponentM)

import Dscp.Launcher.Rio (runRIO)
import Dscp.Resource.Class (AllocResource (..))
import Dscp.Witness.Launcher.Mode (WitnessContext (..), WitnessRealMode)
import Dscp.Witness.Launcher.Params (WitnessParams (..))
import Dscp.Witness.Launcher.Resource (WitnessResources (..))

-- | Make up Witness context from dedicated pack of allocated resources.
formWitnessContext :: WitnessResources -> WitnessContext
formWitnessContext WitnessResources{..} =
    WitnessContext
    { _wcLogging = wrLogging
    , _wcDB = wrDB
    }

runWitnessRealMode :: WitnessContext -> WitnessRealMode a -> IO a
runWitnessRealMode = runRIO

-- | Given params, allocate resources, construct node context and run
-- `WitnessWorkMode` monad.
launchWitnessRealMode :: WitnessParams -> WitnessRealMode a -> IO a
launchWitnessRealMode params action =
    runComponentM "Witness (real mode)" (allocResource params) $
      \resources ->
        let ctx = formWitnessContext resources
        in runWitnessRealMode ctx action

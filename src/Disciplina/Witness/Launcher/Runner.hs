
-- | Helpers for starting an Witness node

module Disciplina.Witness.Launcher.Runner where

import Universum

import Control.Monad.Component (runComponentM)

import Disciplina.Launcher.Resource (AllocResource (..))
import Disciplina.Witness.Launcher.Params (WitnessParams (..))
import Disciplina.Witness.Launcher.Resource (WitnessResources (..))
import Disciplina.Witness.Mode (WitnessContext (..), WitnessRealMode)

-- | Make up Witness context from dedicated pack of allocated resources.
formWitnessContext :: WitnessResources -> WitnessContext
formWitnessContext WitnessResources{..} =
    WitnessContext
    { _wcLoggerName = wrLoggerName
    , _wcDB = wrDB
    }

runWitnessRealMode :: WitnessContext -> WitnessRealMode a -> IO a
runWitnessRealMode ctx action = runReaderT action ctx

-- | Given params, allocate resources, construct node context and run
-- `WitnessWorkMode` monad.
launchWitnessRealMode :: WitnessParams -> WitnessRealMode a -> IO a
launchWitnessRealMode params action =
    runComponentM "Witness (real mode)" (allocResource params) $
      \resources ->
        let ctx = formWitnessContext resources
        in runWitnessRealMode ctx action


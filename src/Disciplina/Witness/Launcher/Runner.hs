
-- | Helpers for starting an Witness node

module Disciplina.Witness.Launcher.Runner where

import Universum

import Disciplina.Launcher.Mode (NodeContext (..))
import Disciplina.Launcher.Resource (BasicNodeResources (..))
import Disciplina.Launcher.Runner (prepareAndRunRealMode)
import Disciplina.Witness.Launcher.Params (WitnessParams (..))
import Disciplina.Witness.Launcher.Resource (WitnessResources (..))
import Disciplina.Witness.Mode (RealMode, Witness, WitnessCustomContext (..))

-- | Make up Witness context from dedicated pack of allocated resources.
formWitnessContext :: WitnessResources -> NodeContext Witness
formWitnessContext WitnessResources{..} =
    NodeContext
    { _ncLoggerName = bnrLoggerName wrBasicResources
    , _ncCustomCtx = WitnessCustomContext
        { _wccDB = wrDB
        }
    }

-- | Given params, allocate resources, construct node context and run
-- `WitnessWorkMode` monad.
launchWitnessRealMode :: WitnessParams -> RealMode Witness a -> IO a
launchWitnessRealMode = prepareAndRunRealMode formWitnessContext


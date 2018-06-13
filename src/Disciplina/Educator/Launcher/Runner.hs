
-- | Helpers for starting an Educator node

module Disciplina.Educator.Launcher.Runner where

import Universum

import Control.Monad.Component (runComponentM)

import Disciplina.Educator.Launcher.Mode (EducatorContext (..), EducatorRealMode)
import Disciplina.Educator.Launcher.Params (EducatorParams (..))
import Disciplina.Educator.Launcher.Resource (EducatorResources (..))
import Disciplina.Launcher.Resource (AllocResource (..))
import Disciplina.Witness.Launcher.Runner (formWitnessContext)

-- | Make up Educator context from dedicated pack of allocated resources.
formEducatorContext :: EducatorResources -> EducatorContext
formEducatorContext EducatorResources{..} =
    EducatorContext
    { _ecWitnessCtx = formWitnessContext erWitnessResources
    }

runEducatorRealMode :: EducatorContext -> EducatorRealMode a -> IO a
runEducatorRealMode ctx action = runReaderT action ctx

-- | Given params, allocate resources, construct node context and run
-- `EducatorWorkMode` monad.
launchEducatorRealMode :: EducatorParams -> EducatorRealMode a -> IO a
launchEducatorRealMode params action =
    runComponentM "Educator (real mode)" (allocResource params) $
      \resources ->
        let ctx = formEducatorContext resources
        in runEducatorRealMode ctx action

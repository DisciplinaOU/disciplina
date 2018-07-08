-- | Helpers for starting an Educator node

module Dscp.Educator.Launcher.Runner where

import Dscp.Educator.Launcher.Mode (EducatorContext (..), EducatorRealMode)
import Dscp.Educator.Launcher.Params (EducatorParams (..))
import Dscp.Educator.Launcher.Resource (EducatorResources (..))
import Dscp.Launcher.Rio (runRIO)
import Dscp.Resource (AllocResource (..), InitParams (..), runResourceAllocation)
import Dscp.Witness.Launcher.Params (wpLoggingParams)
import Dscp.Witness.Launcher.Runner (formWitnessContext)

-- | Make up Educator context from dedicated pack of allocated resources.
formEducatorContext :: EducatorResources -> EducatorContext
formEducatorContext EducatorResources{..} =
    EducatorContext
    { _ecWitnessCtx = formWitnessContext _erWitnessResources
    , _ecDB = _erDB
    , _ecSecret = _erSecret
    }

runEducatorRealMode :: EducatorContext -> EducatorRealMode a -> IO a
runEducatorRealMode = runRIO

-- | Given params, allocate resources, construct node context and run
-- `EducatorWorkMode` monad. Any synchronous exceptions are handled inside.
launchEducatorRealMode :: EducatorParams -> EducatorRealMode () -> IO ()
launchEducatorRealMode params@EducatorParams{..} action =
    void $
    runResourceAllocation appDesc initParams (allocResource params) $
      \resources ->
        let ctx = formEducatorContext resources
        in runEducatorRealMode ctx action
  where
    appDesc = "Educator (real mode)"
    initParams = InitParams{ ipLoggingParams = wpLoggingParams epWitnessParams }

-- | Helpers for starting an Educator node

module Dscp.Educator.Launcher.Runner
    ( formEducatorContext
    , launchEducatorRealMode
    ) where

import Dscp.Educator.Config
import Dscp.Educator.Launcher.Mode (EducatorContext (..), EducatorRealMode)
import Dscp.Educator.Launcher.Params (EducatorParams (..))
import Dscp.Educator.Launcher.Resource (EducatorResources (..))
import Dscp.Launcher.Rio (runRIO)
import Dscp.Resource.Class (AllocResource (..), InitParams (..))
import Dscp.Resource.Functions (runResourceAllocation)
import Dscp.Witness.Launcher (formWitnessContext, wpLoggingParams)

-- | Make up Educator context from dedicated pack of allocated resources.
formEducatorContext :: EducatorParams -> EducatorResources -> IO EducatorContext
formEducatorContext _ecParams _ecResources = do
    _ecWitnessCtx <- formWitnessContext (epWitnessParams _ecParams)
                                        (_erWitnessResources _ecResources)
    pure EducatorContext{..}

-- | Given params, allocate resources, construct node context and run
-- `EducatorWorkMode` monad. Any synchronous exceptions are handled inside.
launchEducatorRealMode
    :: EducatorConfigRec
    -> EducatorParams
    -> (HasEducatorConfig => EducatorRealMode ())
    -> IO ()
launchEducatorRealMode config params@EducatorParams{..} action =
    void $
    withEducatorConfig config $
    runResourceAllocation appDesc initParams (allocResource params) $
        \resources -> do
            ctx <- formEducatorContext params resources
            runRIO ctx action
  where
    appDesc = "Educator (real mode)"
    initParams = InitParams{ ipLoggingParams = wpLoggingParams epWitnessParams }

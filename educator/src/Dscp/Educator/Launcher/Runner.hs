{-# LANGUAGE OverloadedLabels #-}

-- | Helpers for starting an Educator node

module Dscp.Educator.Launcher.Runner
    ( formEducatorContext
    , launchEducatorRealMode
    ) where

import Loot.Config (option, sub)
import Loot.Log (MonadLogging)

import Dscp.Config
import Dscp.Educator.Config
import Dscp.Educator.Launcher.Mode (EducatorContext (..), EducatorRealMode)
import Dscp.Educator.Launcher.Resource (EducatorResources (..))
import Dscp.Resource.Class (AllocResource (..), InitParams (..))
import Dscp.Resource.Functions
import Dscp.Rio (runRIO)
import Dscp.Witness.Launcher

-- | Make up Educator context from dedicated pack of allocated resources.
formEducatorContext
    :: (MonadIO m, MonadCatch m, MonadLogging m, HasEducatorConfig)
    => EducatorResources
    -> m EducatorContext
formEducatorContext _ecResources = do
    let wConfig = rcast educatorConfig
    _ecWitnessVars <- withWitnessConfig wConfig $
        mkWitnessVariables (_erWitnessResources _ecResources)
    pure EducatorContext{..}

-- | Given params, allocate resources, construct node context and run
-- `EducatorWorkMode` monad. Any synchronous exceptions are handled inside.
launchEducatorRealMode
    :: EducatorConfigRec
    -> (HasEducatorConfig => EducatorRealMode ())
    -> IO ()
launchEducatorRealMode config action =
    exitSilentlyOnFailure $
    runResourceAllocation appDesc initParams (allocResource config) $
        \resources -> withEducatorConfig config $ do
            ctx <- formEducatorContext resources
            runRIO ctx action
  where
    appDesc = "Educator (real mode)"
    initParams = InitParams
        { ipLoggingParams = config ^. sub #witness . option #logging
        }

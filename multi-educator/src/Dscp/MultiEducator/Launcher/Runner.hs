{-# LANGUAGE OverloadedLabels #-}

-- | Helpers for starting an Educator node

module Dscp.MultiEducator.Launcher.Runner
    ( formEducatorContext
    , launchEducatorRealMode
    ) where

import Loot.Log (MonadLogging)

import Dscp.Config
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Mode (MultiEducatorContext (..), MultiEducatorRealMode)
import Dscp.MultiEducator.Launcher.Resource (MultiEducatorResources (..))
import Dscp.Resource.Class (AllocResource (..), InitParams (..))
import Dscp.Resource.Functions
import Dscp.Rio (runRIO)
import Dscp.Witness.Launcher

-- | Make up Educator context from dedicated pack of allocated resources.
formEducatorContext
    :: (MonadIO m, MonadCatch m, MonadLogging m, HasMultiEducatorConfig)
    => MultiEducatorResources
    -> m MultiEducatorContext
formEducatorContext _mecResources = do
    let wConfig = rcast multiEducatorConfig
    _mecWitnessVars <- withWitnessConfig wConfig $
        mkWitnessVariables (_merWitnessResources _mecResources)
    pure MultiEducatorContext{..}

-- | Given params, allocate resources, construct node context and run
-- `EducatorWorkMode` monad. Any synchronous exceptions are handled inside.
launchEducatorRealMode
    :: MultiEducatorConfigRec
    -> (HasMultiEducatorConfig => MultiEducatorRealMode ())
    -> IO ()
launchEducatorRealMode config action =
    exitSilentlyOnFailure $
    runResourceAllocation appDesc initParams (allocResource config) $
        \resources -> withMultiEducatorConfig config $ do
            ctx <- formEducatorContext resources
            runRIO ctx action
  where
    appDesc = "Educator (real mode)"
    initParams = InitParams
        { ipLoggingParams = config ^. sub #witness . sub #logging
        }

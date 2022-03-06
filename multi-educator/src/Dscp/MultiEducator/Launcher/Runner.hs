{-# LANGUAGE OverloadedLabels #-}

-- | Helpers for starting an Educator node

module Dscp.MultiEducator.Launcher.Runner
    ( formMultiEducatorContext
    , launchMultiEducatorRealMode
    ) where

import Universum

import Loot.Log (MonadLogging)

import Dscp.Config
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Context
import Dscp.MultiEducator.Launcher.Mode (MultiEducatorContext (..), MultiEducatorRealMode)
import Dscp.MultiEducator.Launcher.Resource ()
import Dscp.Resource.Class (AllocResource (..), InitParams (..))
import Dscp.Resource.Functions
import Dscp.Rio (runRIO)

-- | Make up Educator context from dedicated pack of allocated resources.
formMultiEducatorContext
    :: (MonadIO m, MonadCatch m, MonadLogging m, HasMultiEducatorConfig)
    => MultiEducatorResources
    -> m MultiEducatorContext
formMultiEducatorContext _mecResources = pure MultiEducatorContext{..}

-- | Given params, allocate resources, construct node context and run
-- `MultiEducatorWorkMode` monad. Any synchronous exceptions are handled inside.
launchMultiEducatorRealMode
    :: MultiEducatorConfigRec
    -> (HasMultiEducatorConfig => MultiEducatorRealMode Void)
    -> IO a
launchMultiEducatorRealMode config action =
    exitSilentlyOnFailure $
    runResourceAllocation appDesc initParams (allocResource config) $
        \resources -> withMultiEducatorConfig config $ do
            ctx <- formMultiEducatorContext resources
            runRIO ctx action
  where
    appDesc = "Educator (real mode)"
    initParams = InitParams
        { ipLoggingParams = config ^. sub #educator . sub #logging
        }

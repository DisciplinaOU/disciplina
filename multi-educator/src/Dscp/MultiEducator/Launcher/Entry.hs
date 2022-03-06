{-# LANGUAGE OverloadedLabels #-}

-- | Educator entry point.

module Dscp.MultiEducator.Launcher.Entry
    ( multiEducatorEntry
    ) where

import Universum

import Control.Concurrent (threadDelay)
import Loot.Log (logInfo)
import UnliftIO.Async (async)

import Dscp.MultiEducator.Launcher.Mode
import Dscp.MultiEducator.Web.Server

multiEducatorEntry :: MultiCombinedWorkMode ctx m => m a
multiEducatorEntry = do
    logInfo "Forking student API"
    void $ async serveEducatorAPIsReal
    logInfo "All done"
    forever $ liftIO $ threadDelay 10000000

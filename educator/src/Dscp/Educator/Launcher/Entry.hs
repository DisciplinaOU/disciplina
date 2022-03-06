-- | Educator entry point.

module Dscp.Educator.Launcher.Entry
    ( educatorEntry
    ) where

import Universum
import Control.Concurrent (threadDelay)
import Loot.Log (logInfo)
import UnliftIO.Async (async)

import Dscp.Educator.Launcher.Mode
import Dscp.Educator.Web.Server

-- | Launch witness and educator servers.
serveAPIs :: EducatorWorkMode ctx m => m ()
serveAPIs = do
    logInfo "Forking student API"
    void $ async serveEducatorAPIsReal

-- | Entry point of educator node.
educatorEntry :: FullEducatorWorkMode ctx m => m a
educatorEntry = do
    serveAPIs
    logInfo "All done"
    forever $ liftIO $ threadDelay 10000000

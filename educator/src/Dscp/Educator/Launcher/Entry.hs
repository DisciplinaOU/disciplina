{-# LANGUAGE OverloadedLabels #-}

-- | Educator entry point.

module Dscp.Educator.Launcher.Entry
    ( educatorEntry
    ) where

import Control.Concurrent (threadDelay)
import Loot.Log (logInfo)
import UnliftIO.Async (async)

import Dscp.Config (branch, sub, tree, whenConfigJust)
import Dscp.Educator.Config
import Dscp.Educator.Launcher.Mode
import Dscp.Educator.Web.Server
import Dscp.Educator.Workers
import Dscp.Network
import Dscp.Witness.Launcher
import Dscp.Witness.Web.Server

-- | Listeners, workers and no interaction with user.
withEducatorBackground :: FullEducatorWorkMode ctx m => m a -> m a
withEducatorBackground cont = do
    logInfo "Forking educator workers"
    withWorkers educatorClients cont

-- | Launch witness and educator servers.
serveAPIs :: EducatorWorkMode ctx m => m ()
serveAPIs = do
    let witnessApiParams = witnessConfig ^. sub #witness . sub #api
        educatorApiParams = educatorConfig ^. sub #educator . sub #api . sub #serverParams
        separateWitnessServer =
            witnessApiParams ^. tree #maybe . branch #just
            /= Just educatorApiParams

    whenConfigJust witnessApiParams $ \serverParams ->
        when separateWitnessServer $ do
            logInfo "Forking witness API server"
            void . async $
                serveWitnessAPIReal serverParams

    logInfo "Forking student API"
    void . async $ serveEducatorAPIsReal
        (not separateWitnessServer)

-- | Entry point of educator node.
educatorEntry :: FullEducatorWorkMode ctx m => m a
educatorEntry =
    withServer $
    withWitnessBackground $
    withEducatorBackground $ do
        serveAPIs
        logInfo "All done"
        forever $ liftIO $ threadDelay 10000000

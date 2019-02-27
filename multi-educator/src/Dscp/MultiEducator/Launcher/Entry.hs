{-# LANGUAGE OverloadedLabels #-}

-- | Educator entry point.

module Dscp.MultiEducator.Launcher.Entry
    ( multiEducatorEntry
    ) where

import Control.Concurrent (threadDelay)
import Loot.Log (logInfo)
import UnliftIO.Async (async)

import Dscp.Config (branch, sub, tree, whenConfigJust)
import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Mode
import Dscp.MultiEducator.Web.Server
import Dscp.Network
import Dscp.Witness.Launcher
import Dscp.Witness.Web.Server

multiEducatorEntry :: MultiCombinedWorkMode ctx m => m Void
multiEducatorEntry =
    withServer . withWitnessBackground $ do
        let witnessApiParams = witnessConfig ^. sub #witness . sub #api
            educatorServerParams = multiEducatorConfig ^.
                sub #educator . sub #api . sub #serverParams
            separateWitnessServer =
                witnessApiParams ^. tree #maybe . branch #just /=
                Just educatorServerParams
        whenConfigJust witnessApiParams $ \serverParams ->
            when separateWitnessServer $ do
                logInfo "Forking witness API server"
                void . async $
                    serveWitnessAPIReal serverParams

        logInfo "Forking student API"
        void . async $ serveEducatorAPIsReal
            (not separateWitnessServer)

        logInfo "All done"
        forever $ liftIO $ threadDelay 10000000

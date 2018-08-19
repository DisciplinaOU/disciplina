-- | Educator entry point.

module Dscp.Educator.Launcher.Entry
    ( educatorEntry
    ) where

import Control.Concurrent (threadDelay)
import Loot.Base.HasLens (lensOf)
import Loot.Log (logInfo)
import UnliftIO.Async (async)

import Dscp.Educator.Launcher.Mode
import Dscp.Educator.Launcher.Params
import Dscp.Educator.Web.Params
import Dscp.Educator.Web.Server
import Dscp.Network
import Dscp.Witness.Launcher
import Dscp.Witness.Web.Server

educatorEntry :: CombinedWorkMode ctx m => m ()
educatorEntry =
    withServer $ do
        passiveWitnessEntry

        educatorParams <- view (lensOf @EducatorParams)
        witnessParams <- view (lensOf @WitnessParams)

        let separateWitnessServer =
                wpWitnessServerParams witnessParams /=
                Just (ewpServerParams (epWebParams educatorParams))
        whenJust (wpWitnessServerParams witnessParams) $ \serverParams ->
            when separateWitnessServer $ do
                logInfo "Forking witness API server"
                void . async $
                    serveWitnessAPIReal serverParams

        logInfo "Forking student API"
        void . async $ serveEducatorAPIsReal
            (not separateWitnessServer)
            (epWebParams educatorParams)

        logInfo "All done"
        forever $ liftIO $ threadDelay 10000000

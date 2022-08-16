module Dscp.MultiEducator.Workers
    ( multiEducatorWorkers
    ) where

import qualified Control.Concurrent.STM as STM
import Loot.Base.HasLens (lensOf)
import Loot.Config (option, sub)
import Serokell.Util (modifyTVarS)
import qualified Text.Show
import Time (minute, sec, threadDelay, timeAdd)
import UnliftIO (handle)

import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Context
import Dscp.MultiEducator.Launcher.Educator
import Dscp.MultiEducator.Launcher.Mode
import Dscp.Network
import Dscp.Util.Time

multiEducatorWorkers
    :: MultiEducatorWorkMode ctx m
    => [Client m]
multiEducatorWorkers =
    [ expiredEducatorContextsCleaner
    ]

----------------------------------------------------------------------------
-- Educator contexts expiration
----------------------------------------------------------------------------

-- | Exception used to kill users of expired contexts.
-- Well, as you can guess this should never occur to be thrown eventually.
data TerminatedByContextsCleaner = TerminatedByContextsCleaner
instance Show TerminatedByContextsCleaner where
    show _ = "Thread terminated by contexts cleaner"
instance Exception TerminatedByContextsCleaner

expiredEducatorContextsCleaner :: MultiEducatorWorkMode ctx m => Client m
expiredEducatorContextsCleaner =
    simpleWorker "expiredEducatorsUnload" $
        handleTerminatedException . forever $ cleanup >> nap
  where
    expiryDuration = multiEducatorConfig ^. sub #educator . option #contextExpiry

    handleTerminatedException =
        handle $ \MultiEducatorIsTerminating ->
            -- waiting for this worker to be killed outside
            forever $ threadDelay (minute 1)

    cleanup = do
        educatorContexts <- view $ lensOf @MultiEducatorResources . merEducatorData
        curTime <- getCurTime

        let isExpired ctx = and @[_]
                [ expiryDuration `timeAdd` lecLastActivity ctx > curTime
                , null (lecUsers ctx)
                ]

        ctxs <- atomically . modifyTVarS educatorContexts . onTerminatedThrow $
            gets toList

        toUnload <- forM ctxs $ \ctxVar -> atomically $
            readTVar ctxVar >>= \case
                YetLoadingEducatorContext -> return Nothing
                TerminatingEducatorContext{} -> return Nothing
                FullyLoadedEducatorContext ctx
                    | isExpired ctx -> do
                        let ctx' = ctx{ lecNoFurtherUsers = True }
                        writeTVar ctxVar (FullyLoadedEducatorContext ctx')
                        return $ Just (lecContextKeeper ctx)
                    | otherwise -> return Nothing

        forM_ (catMaybes toUnload) $ \contextKeeper ->
            unloadEducator TerminatedByContextsCleaner contextKeeper

    nap = do
        educatorContexts <- view $ lensOf @MultiEducatorResources . merEducatorData
        curTime <- getCurTime

        let minimalCheckPeriod = sec 1
        let maximalCheckPeriod = sec 10
        let lastActivity = \case
                YetLoadingEducatorContext -> infiniteFuture
                FullyLoadedEducatorContext ctx -> lecLastActivity ctx
                TerminatingEducatorContext{} -> infiniteFuture

        ctxs <- atomically . modifyTVarS educatorContexts . onTerminatedThrow $ do
            ctxs <- gets toList
            maybe (lift STM.retry) pure $ nonEmpty ctxs
            -- Would be nice to check whether any fully loaded context is present here
            -- and 'STM.retry' otherwise, but traversing the whole map atomically
            -- would potentially cause too high contention.
            -- So we keep all 'atomically' blocks no more than constant-size in time.

        lastActivities <- forM ctxs $ fmap lastActivity . readTVarIO

        let nextExpiry = timeAdd expiryDuration (minimum @(NonEmpty _) lastActivities)
        let tillNextExpiry = nextExpiry `timeDiffNonNegative` curTime
        let napTime = min maximalCheckPeriod $ max minimalCheckPeriod tillNextExpiry
        sleep napTime

{-# LANGUAGE OverloadedLists #-}

module Dscp.MultiEducator.Workers
    ( multiEducatorWorkers
    ) where

import qualified Control.Concurrent.STM as STM
import qualified Data.Map as M
import Loot.Base.HasLens (lensOf)
import Loot.Config (option, sub)
import Serokell.Util (modifyTVarS)
import qualified Text.Show
import Time (minute, ms, sec, threadDelay, timeAdd)
import UnliftIO (handle)

import Dscp.MultiEducator.Config
import Dscp.MultiEducator.Launcher.Context
import Dscp.MultiEducator.Launcher.Educator
import Dscp.MultiEducator.Launcher.Mode
import Dscp.Network
import Dscp.Util.Time
import Dscp.Util.TimeLimit

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
    simpleWorker "expiredEducatorsUnload" $ do
        handleTerminatedException $ forever loop
  where
    expiryDuration = multiEducatorConfig ^. sub #educator . option #contextExpiry
    minimalCheckPeriod = sec 1

    lastActivity = \case
        YetLoadingEducatorContext -> infiniteFuture
        FullyLoadedEducatorContext ctx -> lecLastActivity ctx
        TerminatingEducatorContext{} -> infiniteFuture  -- we skip them as non-expired

    handleTerminatedException =
        handle $ \MultiEducatorIsTerminating ->
            -- waiting for this worker to be killed outside
            forever $ threadDelay (minute 1)

    loop = do
        educatorContexts <- view $ lensOf @MultiEducatorResources . merEducatorData

        time <- getCurTime
        let isExpired ctx = and @[_]
                [ expiryDuration `timeAdd` lecLastActivity ctx > time
                , null (lecUsers ctx)
                ]
        let isExpired' = \case
                YetLoadingEducatorContext -> False
                FullyLoadedEducatorContext ctx -> isExpired ctx
                TerminatingEducatorContext{} -> False

        -- Phase 1: remove all expired contexts

        -- under high contention we can get constant retries
        -- so monitoring such possibility
        mask_ . logWarningWaitInf (ms 100) "Educator contexts GC" $ do
            expiredCtxs <- atomically . modifyTVarS educatorContexts . onTerminatedThrow $ do
                ctxMap <- get
                ctxReadMap <- lift $ forM ctxMap $
                    \ctxVar -> (ctxVar, ) <$> readTVar ctxVar
                let (expired, nonExpired) = M.partition (isExpired' . snd) ctxReadMap

                put (fmap fst nonExpired)

                lift . forM expired $ \case
                    (_, YetLoadingEducatorContext) -> error "impossible"
                    (_, TerminatingEducatorContext{}) -> error "impossible"
                    (ctxVar, FullyLoadedEducatorContext ctx) -> do
                        let ctx' = ctx{ lecNoFurtherUsers = True }
                        writeTVar ctxVar $ FullyLoadedEducatorContext ctx'
                        return ctx'

            forM_ expiredCtxs $ \ctx ->
                unloadEducator TerminatedByContextsCleaner (lecContextKeeper ctx)

        -- Phase 2: wait for the nearest expiring context

        do
            nextExpiry <- atomically . modifyTVarS educatorContexts . onTerminatedThrow $ do
                ctxs <- gets M.elems
                las <- lift $ forM ctxs $ fmap lastActivity . readTVar
                when (null las) $ lift STM.retry
                return $ timeAdd expiryDuration (minimum las)

            let tillNextExpiry = nextExpiry `timeDiffNonNegative` time
            sleep $ max minimalCheckPeriod tillNextExpiry

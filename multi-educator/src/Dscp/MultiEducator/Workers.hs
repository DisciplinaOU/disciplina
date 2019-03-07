{-# LANGUAGE OverloadedLists #-}

module Dscp.MultiEducator.Workers
    ( multiEducatorWorkers
    ) where

import qualified Control.Concurrent.STM as STM
import qualified Data.Map as M
import Loot.Base.HasLens (lensOf)
import Loot.Config (option, sub)
import Serokell.Util (modifyTVarS)
import Time (minute, sec, threadDelay, timeAdd)
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
    [ privateBlockCreatorWorker
    ]

----------------------------------------------------------------------------
-- Educator contexts expiration
----------------------------------------------------------------------------

privateBlockCreatorWorker :: MultiEducatorWorkMode ctx m => Client m
privateBlockCreatorWorker =
    simpleWorker "expiredEducatorsUnload" $ do
        handleTerminatedException $ forever loop
  where
    expiryDuration = multiEducatorConfig ^. sub #educator . option #contextExpiry
    minimalCheckPeriod = sec 1

    lastActivity = \case
        LockedEducatorContext -> infiniteFuture
        FullyLoadedEducatorContext ctx -> lecLastActivity ctx

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
                LockedEducatorContext -> False
                FullyLoadedEducatorContext ctx -> isExpired ctx

        -- Phase 1: remove all expired contexts

        -- under high contention we can get constant retries, so monitoring such case
        mask_ . logWarningWaitInf (sec 1) "Educator contexts GC" $ do
            expiredCtxs <- atomically . modifyTVarS educatorContexts . onTerminatedThrow $ do
                ctxMap <- get
                ctxReadMap <- lift $ forM ctxMap $
                    \ctxVar -> (ctxVar, ) <$> readTVar ctxVar
                let (expired, nonExpired) = M.partition (isExpired' . snd) ctxReadMap

                put (fmap fst nonExpired)

                lift . forM expired $ \case
                    (_, LockedEducatorContext) -> error "impossible"
                    (ctxVar, FullyLoadedEducatorContext ctx) -> do
                        let ctx' = ctx{ lecNoFurtherUsers = True }
                        writeTVar ctxVar $ FullyLoadedEducatorContext ctx'
                        return ctx'

            forM_ expiredCtxs unloadEducator

        -- Phase 2: wait for the next expiring context

        logWarningWaitInf (sec 1) "Educator contexts GC sleep" $ do
            nextExpiry <- atomically . modifyTVarS educatorContexts . onTerminatedThrow $ do
                ctxs <- gets M.elems
                las <- lift $ forM ctxs $ fmap lastActivity . readTVar
                when (null las) $ lift STM.retry
                return $ timeAdd expiryDuration (minimum las)

            let tillNextExpiry = nextExpiry `timeDiffNonNegative` time
            sleep $ max minimalCheckPeriod tillNextExpiry

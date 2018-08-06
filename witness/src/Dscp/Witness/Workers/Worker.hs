{-# LANGUAGE OverloadedLists #-}

-- | Node workers

module Dscp.Witness.Workers.Worker
    ( witnessWorkers
    ) where

import Control.Concurrent (threadDelay)
import qualified Data.List.NonEmpty as NE
import Fmt (listF, listF', (+|), (+||), (|+), (||+))
import Loot.Log (logDebug, logError, logInfo, logWarning)

import Dscp.Core
import Dscp.Crypto
import Dscp.Network.Wrapped
import Dscp.Snowdrop.Mode
import Dscp.Util
import Dscp.Witness.Launcher.Mode
import Dscp.Witness.Logic
import Dscp.Witness.Messages


witnessWorkers :: WitnessWorkMode ctx m => [Worker m]
witnessWorkers = [blockUpdateWorker]

----------------------------------------------------------------------------
-- Updates
----------------------------------------------------------------------------

-- | Worker listening to block updates.
blockUpdateWorker :: forall ctx m. WitnessWorkMode ctx m => Worker m
blockUpdateWorker =
    Worker "blockUpdateWorker" [msgType @TipMsg, msgType @BlocksMsg] [subType @PubBlock]
    (\btq -> (bootstrap btq >> action btq) `catchAny` handler)
  where
    handler e = logError $ fromString $ "Exception in blockReceivalWorker " <> show e

    -- We ask for a tip on startup to synchronise.
    bootstrap :: ClientEnv NetTag -> m ()
    bootstrap btq = do
        -- Small delay is needed b/c otherwise simultaneous launch of
        -- several nodes can lead to situation when request is sent to
        -- uninitalised listener of other node.
        liftIO $ threadDelay 200000
        logDebug "Bootstrapped, asking for a tip"
        cliSend btq Nothing GetTipMsg

    action :: ClientEnv NetTag -> m ()
    action btq = do
        logDebug "Started block update worker"

        let processPubBlock nId (PubBlock block) = do
                logInfo $ "New block was published: " +| hashF (headerHash block) |+ ""
                processNewBlock nId btq block
        let processTipMsg nId (TipMsg block) = do
                logInfo $ "Got network tip: " +| hashF (headerHash block) |+ ""
                processNewBlock nId btq block
        let processBlocks _nId (NoBlocksMsg e) = do
                logWarning $ "Processing blocks: got none: " +|| e ||+ ""
            processBlocks _nId (BlocksMsg blocks) = do
                logDebug $ "Processing blocks"
                applyManyBlocks blocks

        forever $ do
            logDebug "blockUpdateWorker: receiving"
            cliRecv btq (-1) [ ccallback processPubBlock
                             , ccallback processTipMsg
                             , ccallback processBlocks
                             ]

    processNewBlock nId btq block = do
        let header = bHeader block
        tip <- runSdM getTipHeader
        let tipHash = headerHash tip
        if | hPrevHash header == tipHash -> applyNewBlock block
           | hDifficulty header > hDifficulty tip -> do
                 logDebug "blockUpdateWorker: requesting blocks to sync"
                 cliSend btq (Just nId) $ GetBlocksMsg tipHash (headerHash block)
           | headerHash header == tipHash -> logDebug "blockUpdateWorker: block is same as our tip"
           | otherwise -> logDebug "blockUpdateWorker: block is useless"

    applyNewBlock block = do
        logDebug $ "Block " +| hashF (headerHash block) |+
                  " is a direct continuation of our tip, applying"
        proof <- applyBlock block
        logInfo $ "Applied received block: " +| block |+
                  " with proof " +|| proof ||+ ", propagating"

    applyManyBlocks blocks = do
        tip <- runSdM getTipBlock
        if (NE.head (unOldestFirst blocks) == tip)
        then do let blocks' = NE.tail $ unOldestFirst blocks
                logDebug $ "Will attempt to apply blocks: " +|
                          listF' hashF (map headerHash blocks) |+ ""
                forM_ blocks' applyBlock
                logInfo $ "Applied received blocks: " +| listF blocks |+ ""
        else logWarning "blockUpdateWorker: received sequence of blocks can't be applied"

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}

-- | Node workers

module Dscp.Witness.Workers.Worker
    ( witnessClients

    , txRetranslatingWorker
    ) where

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE
import Fmt (listF, listF', (+|), (+||), (|+), (||+))
import Loot.Base.HasLens (lensOf)
import Loot.Log (logDebug, logInfo, logWarning)
import Time (ms, sec, threadDelay)
import UnliftIO.Exception (catchJust, withException)

import Dscp.Config (option, sub)
import Dscp.Core
import Dscp.Crypto
import Dscp.Network.Wrapped
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Mode
import Dscp.Util
import Dscp.Util.Concurrent.NotifyWait
import Dscp.Util.Timing
import Dscp.Web.Metrics
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Context
import Dscp.Witness.Logic
import Dscp.Witness.Mempool
import Dscp.Witness.Messages
import Dscp.Witness.Relay
import Dscp.Witness.SDLock

witnessClients
    :: FullWitnessWorkMode ctx m
    => [Client m]
witnessClients =
    [ blockUpdateWorker
    , txRetranslatingWorker
    , networkTxReceivingWorker
    ]

----------------------------------------------------------------------------
-- Updates
----------------------------------------------------------------------------

-- | Worker listening to block updates.
blockUpdateWorker :: forall ctx m. FullWitnessWorkMode ctx m => Client m
blockUpdateWorker =
    set wRecoveryL (constDelay (sec 5)) $
    clientWorker "blockUpdateWorker" [msgType @TipMsg, msgType @BlocksMsg] [subType @PubBlock] $
    \btq -> bootstrap btq >> action btq
  where
    -- We ask for a tip on startup to synchronise.
    bootstrap :: ClientEnv NetTag -> m ()
    bootstrap btq = do
        -- Small delay is needed b/c otherwise simultaneous launch of
        -- several nodes can lead to situation when request is sent to
        -- uninitalised listener of other node.
        liftIO $ threadDelay (ms 200)
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

        logDebug "blockUpdateWorker: receiving"
        cliRecv btq (-1) [ ccallback processPubBlock
                         , ccallback processTipMsg
                         , ccallback processBlocks
                         ]

    processNewBlock nId btq block = do
        let header = bHeader block
        tip <- runSdMLocked getTipHeader
        let tipHash = headerHash tip
        if | hPrevHash header == tipHash -> applyNewBlock block
           | hDifficulty header > hDifficulty tip -> do
                 logDebug "blockUpdateWorker: requesting blocks to sync"
                 cliSend btq (Just nId) $ GetBlocksMsg tipHash (headerHash block)
           | headerHash header == tipHash -> logDebug "blockUpdateWorker: block is same as our tip"
           | otherwise -> logDebug "blockUpdateWorker: block is useless"

    applyNewBlock block =
        writingSDLock "apply new block" $ do
            let metricsEndpoint = witnessConfig ^. sub #witness . option #metricsEndpoint
            logDebug $ "Block " +| hashF (headerHash block) |+
                      " is a direct continuation of our tip, applying"
            proof <- reportTime "disciplina.timer.block_apply" metricsEndpoint $
                applyBlock block
            logInfo $ "Applied received block: " +| block |+
                      " with proof " +|| proof ||+ ", propagating"
            rejectedTxs <- normalizeMempool
            logInfo $ "Normalizing mempool by the way, lost transactions: "
                      +| listF (onlyLostTxs rejectedTxs (one $ bBody block)) |+ ""

    applyManyBlocks blocks =
        writingSDLock "apply many blocks" $ do
            tip <- runSdM getTipBlock
            if (NE.head (unOldestFirst blocks) == tip)
            then do
                let blocks' = NE.tail $ unOldestFirst blocks
                logDebug $ "Will attempt to apply blocks: " +|
                          listF' hashF (map headerHash blocks) |+ ""
                forM_ blocks' applyBlock
                logInfo $ "Applied received blocks: " +| listF blocks |+ ""
                rejectedTxs <- normalizeMempool
                logInfo $ "Normalized mempool, lost transactions: "
                          +| listF (onlyLostTxs rejectedTxs (map bBody blocks')) |+ ""
            else logWarning "blockUpdateWorker: received sequence of blocks can't be applied"

----------------------------------------------------------------------------
-- Retranslator, input part
----------------------------------------------------------------------------

-- | Check that the transaction is neither failed nor in a mempool and republish it.
checkThenRepublish
    :: WitnessWorkMode ctx m
    => GTxWitnessed -> m ()
checkThenRepublish tx = do
    RelayState _input output failedTxs <- view (lensOf @RelayState)

    hashmap <- atomically $ readTVar failedTxs

    let hasFailed = hash tx `HashMap.lookup` hashmap
    whenJust hasFailed $ \(_, e) ->
        throwM e

    writingSDLock "add to mempool" $
        addTxToMempool tx `withException` addFailedTx failedTxs

    atomically $ STM.writeTBQueue output tx
  where
    addFailedTx failedTxs e =
        atomically $ STM.modifyTVar failedTxs $ HashMap.insert (hash tx) (tx, e)

-- | Reads transaction queue and publishes transactions.
txRetranslatingWorker
    :: WitnessWorkMode ctx m
    => Client m
txRetranslatingWorker =
    set wRecoveryL retryOnSpot $
    simpleWorker "txRetranslationInitialiser" $ do
        relay <- view (lensOf @RelayState)
        (tx, inMempoolNotifier) <- atomically $ STM.readTBQueue (_rsInput relay)
        checkThenRepublish tx
            & finallyNotify inMempoolNotifier
            & handleAny (\e -> logDebug $ "Transaction failed: " +| e |+ "")

-- | Handles incoming transactions.
networkTxReceivingWorker :: FullWitnessWorkMode ctx m => Client m
networkTxReceivingWorker =
    set wRecoveryL (constDelay (sec 1)) $
        clientWorker "txRetranslationRepeater" [] [subType @PubTx] $ \btq -> do
            (_, PubTx tx) <- cliRecvUpdate btq (-1)
            checkThenRepublish tx
                & handleAlreadyExists (\_ -> pass)
                & handleAny (\e -> logWarning $
                                  "Exception in republisher: " +| e |+ "")
  where
    handleAlreadyExists = flip $ catchJust (^? _TransactionAlreadyExists)

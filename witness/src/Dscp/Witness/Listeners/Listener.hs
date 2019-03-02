{-# LANGUAGE OverloadedLists #-}

-- | Node Listeners

module Dscp.Witness.Listeners.Listener
    ( witnessListeners
    ) where

import qualified Control.Concurrent.STM as STM
import Fmt ((+|), (+||), (|+), (||+))
import Time (sec)

import Loot.Base.HasLens (HasLens (..))
import Loot.Log (logDebug, logInfo)

import Dscp.Core
import Dscp.Network.Wrapped
import Dscp.Resource.Keys (ourPublicKey)
import Dscp.Snowdrop
import Dscp.Util.Timing
import Dscp.Witness.Launcher.Context
import Dscp.Witness.Logic
import Dscp.Witness.Messages
import Dscp.Witness.Relay
import Dscp.Witness.SDLock


witnessListeners :: FullWitnessWorkMode ctx m => [Worker m]
witnessListeners =
    [ blockIssuingListener
    , getBlocksListener
    , getTipListener
    , txPublisher
    ]

----------------------------------------------------------------------------
-- Block creation
----------------------------------------------------------------------------

blockIssuingListener :: forall ctx m. FullWitnessWorkMode ctx m => Worker m
blockIssuingListener =
    simpleListener "blockIssuingListener" [] $ \btq ->
        recoverAll "BlockIssuingListener" (constDelay (sec 2)) (action btq)
  where
    action :: ListenerEnv NetTag -> m ()
    action btq = do
        let GovCommittee committee = genesisGovernance
        slotId <- waitUntilNextSlot
        ourAddr <- mkAddr <$> ourPublicKey @WitnessNode
        logInfo $ "New slot has just started: " +| slotId |+ ""
        if committeeOwnsSlot committee ourAddr slotId
        then issueBlock slotId
        else logInfo "We don't own current slot, skipping"
      where
        issueBlock slotId = do
            (block, proof) <-
                writingSDLock "create & apply block" $ do
                    block <- createBlock slotId
                    logInfo $ "Created a new block: \n" +| block |+ ""
                    proof <- applyBlock block
                    return (block, proof)
            logInfo $ "Applied block, proof: " +|| proof ||+ ", propagating"
            atomically $ servPub btq (PubBlock block)

----------------------------------------------------------------------------
-- Headers and blocks
----------------------------------------------------------------------------

getBlocksListener :: FullWitnessWorkMode ctx m => Worker m
getBlocksListener =
    callbacksListener "getHeadersListener" [msgType @GetBlocksMsg] $ \btq ->
        [lcallback (respond btq)]
  where
    respond btq cliId (GetBlocksMsg{..}) = do
        logDebug "getBlocksMsg: received request"
        res <- runSdMLocked $ getBlocksFromTo gbOlder gbNewer
        let response = either NoBlocksMsg BlocksMsg res
        atomically $ servSend btq cliId response
        logDebug "getBlocksMsg: response sent"

getTipListener :: FullWitnessWorkMode ctx m => Worker m
getTipListener =
    callbacksListener "getTipListener" [msgType @GetTipMsg] $ \btq ->
        [lcallback (respond btq)]
  where
    respond btq cliId GetTipMsg = do
        logDebug "getTipMsg: received request"
        tip <- runSdMLocked getTipBlock
        atomically $ servSend btq cliId (TipMsg tip)
        logDebug "getTipMsg: response sent"

----------------------------------------------------------------------------
-- Retranslator, publishing part
----------------------------------------------------------------------------

txPublisher :: FullWitnessWorkMode ctx m => Worker m
txPublisher = simpleListener
    "txRetranslationPublisher"
    [] $ \btq -> do
        recoverAll "Tx publisher" retryOnSpot $ do
            RelayState _ output _ <- view $ lensOf @RelayState
            atomically $ do
                tx <- STM.readTBQueue output
                servPub btq (PubTx tx)

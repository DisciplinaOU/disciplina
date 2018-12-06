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


witnessListeners :: WitnessWorkMode ctx m => m [Listener m]
witnessListeners = do
    relayState <- view (lensOf @RelayState)
    return
        [ blockIssuingListener
        , getBlocksListener
        , getTipListener
        , txPublisher relayState
        ]

----------------------------------------------------------------------------
-- Block creation
----------------------------------------------------------------------------

blockIssuingListener :: forall ctx m. WitnessWorkMode ctx m => Listener m
blockIssuingListener =
    Listener "blockIssuingListener" [] $ \btq ->
        forever $ recoverAll "BlockIssuingListener" (constDelay (sec 2)) (action btq)
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

getBlocksListener :: WitnessWorkMode ctx m => Listener m
getBlocksListener =
    simpleListener "getHeadersListener" [msgType @GetBlocksMsg] $ \btq ->
        [lcallback (respond btq)]
  where
    respond btq cliId (GetBlocksMsg{..}) = do
        logDebug "getBlocksMsg: received request"
        res <- runSdMLocked $ getBlocksFromTo gbOlder gbNewer
        let response = either NoBlocksMsg BlocksMsg res
        atomically $ servSend btq cliId response
        logDebug "getBlocksMsg: response sent"

getTipListener :: WitnessWorkMode ctx m => Listener m
getTipListener =
    simpleListener "getTipListener" [msgType @GetTipMsg] $ \btq ->
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

txPublisher :: WitnessWorkMode ctx m => RelayState -> Listener m
txPublisher (RelayState _ pipe _) = Listener
    "txRetranslationPublisher"
    [] $ \btq -> do
        forever . recoverAll "Tx publisher" retryOnSpot $ atomically $ do
            tx <- STM.readTBQueue pipe
            servPub btq (PubTx tx)

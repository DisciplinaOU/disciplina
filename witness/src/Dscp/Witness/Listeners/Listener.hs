{-# LANGUAGE OverloadedLists #-}

-- | Node Listeners

module Dscp.Witness.Listeners.Listener
    ( witnessListeners
    ) where

import Fmt ((+|), (|+))
import Loot.Log (logDebug, logError, logInfo)

import Dscp.Core
import Dscp.Network.Wrapped
import Dscp.Resource.Keys (ourPublicKey)
import Dscp.Snowdrop
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Marker
import Dscp.Witness.Launcher.Mode
import Dscp.Witness.Logic
import Dscp.Witness.Messages


witnessListeners
    :: forall ctx m. WitnessWorkMode ctx m
    => [Listener m]
witnessListeners = [blockIssuingListener, getBlocksListener, getTipListener]

----------------------------------------------------------------------------
-- Block creation
----------------------------------------------------------------------------

blockIssuingListener :: forall ctx m. WitnessWorkMode ctx m => Listener m
blockIssuingListener =
    Listener "blockIssuingListener" [] (\btq -> action btq `catchAny` handler)
  where
    handler e = logError $ fromString $ "Exception in blockIssuingListener: " <> show e
    action :: ListenerEnv NetTag -> m ()
    action btq = forever $ do
        let GovCommittee committee = gcGovernance $ giveL @WitnessConfig @GenesisConfig
        slotId <- waitUntilNextSlot
        ourAddr <- mkAddr <$> ourPublicKey @WitnessNode
        logInfo $ "New slot has just started: " +| slotId |+ ""
        if committeeOwnsSlot committee ourAddr slotId
        then issueBlock slotId
        else logInfo "We don't own current slot, skipping"
      where
        issueBlock slotId = do
            block <- createBlock slotId
            logInfo $ "Created a new block: \n" +| block |+ ""
            proof <- applyBlock block
            logInfo $ "Applied block, proof: " +| proof |+ ", propagating"
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
        res <- runSdM $ getBlocksFromTo gbFrom gbTo
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
        tip <- runSdM getTipBlock
        atomically $ servSend btq cliId (TipMsg tip)
        logDebug "getTipMsg: response sent"

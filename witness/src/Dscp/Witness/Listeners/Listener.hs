{-# LANGUAGE OverloadedLists #-}

-- | Node Listeners

module Dscp.Witness.Listeners.Listener
    ( witnessListeners
    ) where

import Fmt ((+||), (||+))
import Loot.Log (logError, logInfo)

import Dscp.Core
import Dscp.Network.Wrapped
import Dscp.Resource.Keys (ourPublicKey)
import Dscp.Witness.Block.Logic (applyBlock, createBlock)
import Dscp.Witness.Config
import Dscp.Witness.Launcher
import Dscp.Witness.Messages


witnessListeners
    :: forall ctx m. WitnessWorkMode ctx m
    => [Listener m]
witnessListeners = [blockIssuingListener]

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
        logInfo $ "New slot has just started: " +|| slotId ||+ ""
        if committeeOwnsSlot committee ourAddr slotId
        then issueBlock
        else logInfo "We don't own current slot, skipping"
      where
        issueBlock = do
            block <- createBlock
            logInfo "Created a new block"
            proof <- applyBlock block
            logInfo $ "Applied block, proof: " +|| proof ||+ ", propagating"
            atomically $ servPub btq (PubBlock block)

----------------------------------------------------------------------------
-- Ping/pong listeners
----------------------------------------------------------------------------

_blkListener, _txListener :: WitnessWorkMode ctx m => Listener m
_blkListener =
    simpleListener "blkListener" [msgType @PingBlk] $ \btq ->
    let blkCallback cId PingBlk = do
            logInfo "got PingBlk"
            atomically $ servSend btq cId (PongBlk "that was a great block")
            logInfo "got PingBlk, replied"
    in [ lcallback blkCallback ]
_txListener =
    simpleListener "txListener" [msgType @PingTx] $ \btq ->
    let txCallback cId PingTx = do
            logInfo "got PingTx"
            atomically $ servSend btq cId (PongTx "wonderful tx, thank you!")
            logInfo "got PingTx, replied"
    in [ lcallback txCallback ]

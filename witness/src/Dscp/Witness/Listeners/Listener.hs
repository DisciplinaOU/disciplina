{-# LANGUAGE OverloadedLists #-}

-- | Node Listeners

module Dscp.Witness.Listeners.Listener
    ( witnessListeners
    ) where

import Fmt ((+|), (|+))
import Loot.Log (logError, logInfo)

import Dscp.Core
import Dscp.Network.Wrapped
import Dscp.Resource.Keys (ourPublicKey)
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Marker
import Dscp.Witness.Launcher.Mode
import Dscp.Witness.Logic (applyBlock, createBlock)
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

{-# LANGUAGE OverloadedLists #-}

-- | Node workers

module Dscp.Workers.Worker
    ( witnessWorkers
    ) where

import Universum

import Loot.Log (logInfo, logWarning, modifyLogName)
import Loot.Network.Class (ClientEnv, ClientId, MsgType, Subscription)
import Loot.Network.Class (CliId, Content, ListenerEnv, ListenerId, MsgType, ServSendMsg (..))

import Dscp.Network.Messages (PingBlk (..), PingTx (..), PongBlk (..), PongTx (..))
import Dscp.Network.Wrapped (Worker (..), cliRecvResp, cliSend, msgType)
import Dscp.Witness.Launcher (WitnessWorkMode)

witnessWorkers :: WitnessWorkMode m => [Worker t m]
witnessWorkers = [witnessTxWorker]

witnessTxWorker :: forall t m. WitnessWorkMode m => Worker t m
witnessTxWorker = Worker "txWorker" [msgType @PongTx] [] action
  where
    action :: ClientEnv t -> m ()
    action btq = forever $ do
        cliSend @t btq Nothing PingTx
        (nId,PongTx txt) <- cliRecvResp @t btq (-1)
        logInfo $ fromString $ "Heard pongtx: " <> show txt

witnessBlkWorker :: forall t m. WitnessWorkMode m => Worker t m
witnessBlkWorker = Worker "blkWorker" [msgType @PongBlk] [] action
  where
    action :: ClientEnv t -> m ()
    action btq = forever $ do
        cliSend @t btq Nothing PingBlk
        (nId,PongBlk txt) <- cliRecvResp @t btq (-1)
        logInfo $ fromString $ "Heard pongblk: " <> show txt

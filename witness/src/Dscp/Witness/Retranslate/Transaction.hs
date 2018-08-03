{-# LANGUAGE OverloadedLists #-}

module Dscp.Witness.Retranslate.Transaction
    (
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TQueue, TVar, modifyTVar, newTQueueIO, readTQueue, readTVar,
                               writeTQueue, writeTVar)

import qualified Data.HashMap.Strict as HashMap

import Fmt ((+|), (+||), (|+), (||+))

import Loot.Base.HasLens (HasLens (..))
import Loot.Log (logError, logInfo)
import Loot.Network.Class (ClientEnv, getPeers)
import Loot.Network.ZMQ (ZmqTcp)

import Dscp.Core
import Dscp.Crypto
import Dscp.Network.Wrapped
import Dscp.Witness.Logic
import Dscp.Witness.Launcher.Mode
import Dscp.Witness.Mempool
import Dscp.Witness.Messages

makeRetranslator :: forall ctx m. WitnessWorkMode ctx m => IO (Listener m, Worker m)
makeRetranslator = do
    localState <- newTVarIO HashMap.empty
    queue      <- newTQueueIO

    return (mkListener localState queue, mkWorker localState queue)
  where
    mkListener failedTxs queue =
        Listener "transactionRetranslatorOutput" [] $ \btq -> do
            dieGracefully $ do
                forever $ do
                    tx <- atomically $ readTQueue queue
                    atomically $ servPub btq (RetranslateTx tx)

    mkWorker failedTxs queue =
        Worker "transactionRetranslatorInput" [] [subType @RetranslateTx] $ \btq -> do
            dieGracefully $ do
                forever $ do
                    (_, RetranslateTx tx) <- cliRecvUpdate btq (-1)
                    hashmap               <- atomically $ readTVar failedTxs

                    unless (hash tx `elem` hashmap) $ do
                        pool    <- view (lensOf @MempoolVar @ctx)
                        isThere <- isInMempool pool tx

                        unless isThere $ do
                            addTxToMempool pool tx
                            atomically $ writeTQueue queue tx

    dieGracefully action =
        action `catchAny` \e -> do
            logError $ fromString $ "Exception in transactionRetranslatorInput: " <> show e

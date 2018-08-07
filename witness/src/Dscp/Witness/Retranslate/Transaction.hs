{-# LANGUAGE OverloadedLists #-}

module Dscp.Witness.Retranslate.Transaction
    ( TxRetranslator (..)
    , makeRetranslator
    ) where

import qualified Control.Concurrent.STM as STM

import qualified Data.HashMap.Strict as HashMap

import Fmt ((+|), (+||), (|+), (||+))

import Loot.Base.HasLens (HasLens (..))
import Loot.Log (logError, logInfo)
import Loot.Network.Class (ClientEnv, getPeers)
import Loot.Network.ZMQ (ZmqTcp)

import Dscp.Crypto (hash)
import Dscp.Network.Wrapped (Listener (..), Worker (..), cliRecvUpdate, msgType, servPub, subType)
import Dscp.Witness.Launcher.Mode (WitnessWorkMode)
import Dscp.Witness.Mempool (MempoolVar, addTxToMempool, isInMempool)
import Dscp.Witness.Messages (PubTx (..), SendTx (..))

import qualified Loot.Network.BiTQueue as BTQ

-- | Holds input queue for 'SendTx's and all moving parts of the retranslator.
--
--   'Worker's/'Listener's are there because we might at some point want to stop
--   this mechanism gracefully.
--
--   They could be replaced by some `trStop :: m ()` method.
--
data TxRetranslator m
    = TxRetranslator
        { trInput           :: STM.TQueue SendTx  -- ^ 'TQueue' messages arrive from
        , trMessageConsumer :: Worker m           -- ^ Watches for messages
        , trSubConsumer     :: Worker m           -- ^ Watches other retranslations
        , trRepublisher     :: Listener m         -- ^ Publishes txs
        }

-- | The idea is as follows:
--
--   1) We have two "workers" - one for messages and one is subscribed on republications.
--      These two cannot be put into one 'STM' action, sadly, because `cliRecvUpdate`
--      is not 'STM' and reading bytestring from `btq` directly is not what we want
--      on this abstraction level.
--
--   2) Each of these workers receives txs to retranslate from its respective source,
--      checks that they are applicable to retranslation and pushes them into the pipe.
--
--   3) Republisher drains the pipe and publishes everything that comes from it.
--
--   This is the connection scheme of the retranslator:
--
--   other 'PubTx's --> trSubConsumer --+
--                                      |
--                                      V
--   input --> trMessageConsumer --> pipe --> trRepublisher
--
makeRetranslator :: forall ctx m. WitnessWorkMode ctx m => STM (TxRetranslator m)
makeRetranslator = do
    failedTxs <- STM.newTVar HashMap.empty
    input     <- STM.newTQueue
    pipe      <- STM.newTQueue

    return (retranslator failedTxs input pipe)
  where
    retranslator failedTxs input pipe =  TxRetranslator
        {
          trInput = input

        , trMessageConsumer = Worker
            "txRetranslationInitialiser"
            [msgType @SendTx]
            [subType @PubTx] $ \btq -> do
                dieGracefully $ do
                    forever $ do
                        SendTx tx <- sync $ STM.readTQueue input
                        checkThenRepublish tx

        , trSubConsumer = Worker
            "txRetranslationRepeater"
            [msgType @SendTx]
            [subType @PubTx] $ \btq -> do
                dieGracefully $ do
                    forever $ do
                        (_, PubTx tx) <- cliRecvUpdate btq (-1)
                        checkThenRepublish tx

        , trRepublisher = Listener
            "txRetranslationPublisher"
            [] $ \btq -> do
                dieGracefully $ do
                    forever $ sync $ do
                        tx <- STM.readTQueue pipe
                        servPub btq (PubTx tx)
        }
      where
        -- Check that the transaction is neither failed nor in a mempool and republish it.
        checkThenRepublish tx = do
            hashmap <- sync $ readTVar failedTxs

            unless (hash tx `elem` hashmap) $ do
                pool    <- view (lensOf @MempoolVar @ctx)
                isThere <- isInMempool pool tx

                unless isThere $ do
                    addTxToMempool pool tx
                    sync $ STM.writeTQueue pipe tx

        -- Catch error and log it.
        -- TODO: move to some *.Utils.
        dieGracefully action =
            action `catchAny` \e -> do
                logError $ fromString $ "Exception in transactionRetranslatorInput: " <> show e

-- | Run STM action inside (MonadIO m).
--   TODO: move to some *.Utils.
sync :: MonadIO m => STM.STM a -> m a
sync = liftIO . STM.atomically


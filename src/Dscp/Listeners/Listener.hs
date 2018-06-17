{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Node Listeners

module Dscp.Listeners.Listener where

import Universum

import Codec.Serialise (DeserialiseFailure, serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as Set
import Loot.Network.BiTQueue (BiTQueue, recvBtq, sendBtq)
import Loot.Network.Class (CliId, Content, ListenerEnv, ListenerId, MsgType, ServSendMsg (..))
import Loot.Network.Message (CallbackWrapper (..), Message, handlerDecoded, runCallbacksInt)

import Dscp.Network.Messages (PingBlk (..), PingTx (..), PongBlk (..), PongTx (..), fromMsgType,
                              getMsgType)
import Dscp.Witness.Launcher (WitnessWorkMode)

data Listener t m = Listener
    { lId       :: ListenerId
    , lMsgTypes :: Set MsgType
    , lAction   :: ListenerEnv t -> m ()
    }

dispatch ::
       forall t m. Monad m
    => [CallbackWrapper (CliId t) m ()]
    -> (CliId t, MsgType, Content)
    -> m ()
dispatch callbacks (cliId,msgT,content) = case (fromMsgType msgT,content) of
      (Just n,d:_) -> runCallbacksInt callbacks n d cliId
      _            -> pass

simpleListener ::
       forall t m s. MonadIO m
    => [CallbackWrapper (CliId t) m ()]
    -> BiTQueue (CliId t, MsgType, Content) s
    -> m ()
simpleListener callbacks btq  =
    forever $ atomically (recvBtq btq) >>= dispatch @t callbacks

callback ::
       forall t d m. (Message d, Monad m)
    => (CliId t -> d -> m ())
    -> CallbackWrapper (CliId t) m ()
callback foo = handlerDecoded $ \cId -> either (const $ pass) (foo cId)

reply :: forall t r d m. (MonadIO m, Message d) =>
         BiTQueue r (ServSendMsg (CliId t)) -> CliId t -> d -> m ()
reply btq cId m =
    atomically $
    sendBtq btq (Reply cId (getMsgType @d) [BSL.toStrict $ serialise m])

-- DSCP-105 TODO
witnessListeners
    :: forall m t. WitnessWorkMode m
    => [Listener t m]
witnessListeners = [blkListener, txListener]
   where

     blkListener = Listener "blkListener" (Set.fromList [getMsgType @PingBlk]) $ \btq ->
         let blkCallback (cId :: CliId t) PingBlk = reply @t btq cId (PongBlk "")
         in simpleListener @t [ callback @t blkCallback ] btq

     txListener = undefined
--    blkListener :: Listener Packing BS.ByteString m
--    blkListener = Listener $ \_ _peerId (cactions :: ConversationActions PongBlk PingBlk m) -> do
--        logInfo "heard Blk"
--        send cactions (PongBlk "")
--    txListener :: Listener Packing BS.ByteString m
--    txListener = Listener $ \_ _peerId (cactions :: ConversationActions PongTx PingTx m) -> do
--        logInfo "heard Tx"
--        send cactions (PongTx "")

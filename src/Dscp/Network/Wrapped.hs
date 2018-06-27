{-# LANGUAGE ExistentialQuantification #-}

-- Some of these methods have 'NetworkingX' constraint added though it is
-- not required. I use it to force fundep m -> t so user can use methods
-- without specifying t.

-- | High-level typed wrapper over loot-network, is supposed to be
-- used within the library instead of loot-network directly.

module Dscp.Network.Wrapped
    ( MsgK
    , SubK
    , msgType
    , fromMsgType
    , subType
    , fromSubType

    , Listener (..)
    , runListener
    , servSend
    , servPub
    , simpleListener
    , lcallback

    , Worker (..)
    , runWorker
    , cliSend
    , CliRecvExc(..)
    , cliRecv
    , cliRecvResp
    , cliRecvUpdate

    , withClient
    , withServer
    ) where


import Universum

import Codec.Serialise (serialise)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (orElse, retry)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, putTMVar, readTMVar)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Loot.Log (MonadLogging, logDebug, logError, logInfo, logWarning)
import Loot.Network.BiTQueue (recvBtq, sendBtq)
import Loot.Network.Class (CliId, ClientEnv, ClientId, ListenerEnv, ListenerId, MsgType (..),
                           NetworkingCli, NetworkingServ, NodeId, Subscription (..), registerClient,
                           registerListener, runClient, runServer)
import qualified Loot.Network.Class as L
import Loot.Network.Message (CallbackWrapper, Message (..), getMsgTag, handlerDecoded,
                             runCallbacksInt)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (withAsync)

-- | Tag for messages which are "real communication messages".
data MsgK

-- | Tag for subscription messages.
data SubK

-- | Get a 'MsgType' related to the message specified (pack the
-- natural which is related to the message type).
msgType :: forall d. (Message MsgK d) => MsgType
msgType = MsgType $ BS8.pack $ show $ getMsgTag @MsgK @d

-- | Convert 'MsgType' to a natural number.
fromMsgType :: MsgType -> Maybe Natural
fromMsgType (MsgType bs) = readMaybe (BS8.unpack bs)

-- | Same as 'msgType', but for subscriptions.
subType :: forall d. (Message SubK d) => Subscription
subType = Subscription $ BS8.pack $ show $ getMsgTag @SubK @d

-- | Same as 'fromMsgType', but for subscriptions.
fromSubType :: Subscription -> Maybe Natural
fromSubType (Subscription bs) = readMaybe (BS8.unpack bs)

---------------------------------------------------------------------------
-- Listeners
----------------------------------------------------------------------------

-- Listeners are supposed to have one dispatcher only (and one receive
-- block only). When new messages come, response callback thread is
-- forked and this thread is allowed to send multiple messages
-- (directly, or publish).

data Listener t m = Listener
    { lId       :: !ListenerId
      -- ^ Listener id, should be unique.
    , lMsgTypes :: !(Set MsgType)
      -- ^ Message types listener is supposed to receive.
    , lAction   :: !(ListenerEnv t -> m ())
      -- ^ Listener's action.
    }

runListener ::
       forall t m n. (NetworkingServ t n, MonadLogging n, MonadMask n)
    => (forall x. m x -> n x)
    -> Listener t m
    -> n ()
runListener nat Listener{..} = do
    logDebug $ fromString $ "Launching listner " <> show lId
    (lEnv :: ListenerEnv t) <- registerListener @t lId lMsgTypes
    nat (lAction lEnv) `finally` (logDebug $ fromString $ "Listener " <> show lId <> " has exited")

servSend :: forall t d. Message MsgK d => ListenerEnv t -> CliId t -> d -> STM ()
servSend btq cliId msg =
    sendBtq btq $ L.Reply cliId (msgType @d) [BSL.toStrict $ serialise msg]

servPub :: forall t d. Message SubK d => ListenerEnv t -> d -> STM ()
servPub btq msg =
    sendBtq btq $ L.Publish (subType @d) [BSL.toStrict $ serialise msg]

simpleListener ::
       forall t m. (MonadIO m, MonadCatch m, MonadLogging m)
    => ListenerId
    -> Set MsgType
    -> (ListenerEnv t -> [CallbackWrapper (CliId t) m ()])
    -> Listener t m
simpleListener lId lMsgTypes getCallbacks =
    Listener {..}
  where
    -- todo use 'fmt' or something similar
    lAction btq = do
        logDebug $ fromString $ "Listener " <> show lId <> " has started."
        forever $ action btq `catchAny` handler
    action btq = do
        let callbacks = getCallbacks btq
        (cliId,msgT,content) <- atomically $ recvBtq btq
        case (fromMsgType msgT,content) of
            (Just n,[d]) -> runCallbacksInt callbacks n d cliId >>= \case
                Nothing -> logWarning $ fromString $ "Listener " <> show lId <>
                                                     "couldn't match on type (runCallbacksInt)"
                _       -> pass
            _            -> pass
    handler e = do
        logError $ fromString $
            "Listener " <> show lId <> " has failed with an error: " <>
            show e <> ". Recovering (in 2sec)."
        liftIO $ threadDelay $ 2000000

-- for server, we just skip the message if we can't decode it, since
-- we have only one dispatcher.
lcallback ::
       forall t d m. (Message MsgK d, Monad m)
    => (CliId t -> d -> m ())
    -> CallbackWrapper (CliId t) m ()
lcallback foo = handlerDecoded $ \cId -> either (const $ pass) (foo cId)

----------------------------------------------------------------------------
-- Workers
----------------------------------------------------------------------------

data Worker t m = Worker
    { wId       :: !ClientId
      -- ^ Worker's identity.
    , wMsgTypes :: !(Set MsgType)
      -- ^ Message types worker is supposed to receive.
    , wSubs     :: !(Set Subscription)
      -- ^ Worker's subscriptions.
    , wAction   :: !(ClientEnv t -> m ())
      -- ^ Worker's action.
    }

runWorker ::
       forall t m n. (NetworkingCli t n, MonadMask n, MonadLogging n)
    => (forall x. m x -> n x)
    -> Worker t m
    -> n ()
runWorker nat Worker{..} = do
    logDebug $ fromString $ "Launching worker " <> show wId
    (cEnv :: ClientEnv t) <- registerClient @t wId wMsgTypes wSubs
    nat (wAction cEnv) `finally` (logDebug $ fromString $ "Worker " <> show wId <> " has exited")

cliSend ::
       forall t d m. (Message MsgK d, NetworkingCli t m, MonadIO m)
    => ClientEnv t
    -> Maybe (NodeId t)
    -> d
    -> m ()
cliSend btq nId msg =
    atomically $ sendBtq btq (nId, (msgType @d, [BSL.toStrict $ serialise msg]))

data CliRecvExc
    = CRETimeout
    | CREUnexpected Text
    | CREMalformed Text
    deriving (Eq, Show, Generic)

instance Exception CliRecvExc

data CliRecvExcInternal
    = CREMalformedTag
    | CRENoCallback Natural
    | CREWrongFramesNumber Int
    deriving (Eq,Show,Generic)

instance Exception CliRecvExcInternal

type SendConstraint k t d m
     = ( Message k d
       , NetworkingCli t m
       , MonadUnliftIO m
       , MonadCatch m
       , MonadLogging m)

-- Timeout -- milliseconds, 0 if instant response is expected, -1 (any
-- negative) if timeout should be disabled.
--
-- Callback takes care of decoding itself because we might want to
-- propagate data first (before decoding the message).
cliRecv ::
       forall t k d m a. SendConstraint k t d m
    => ClientEnv t
    -> Int
    -> [CallbackWrapper (NodeId t) m a]
    -> m a
cliRecv btq timeout callbacks = withHandler $ withTimeout $ \tmAction -> do
    res <- atomically $ (Right <$> recvBtq btq) `orElse` (Left <$> tmAction)
    let call nId msgTagM msgs = do
            msgTag <- maybe (throwM CREMalformedTag) pure msgTagM
            msg <- case msgs of
                [x]   -> pure x
                other -> throwM $ CREWrongFramesNumber (length other)
            runCallbacksInt callbacks msgTag msg nId >>= \case
                Nothing -> throwM $ CRENoCallback msgTag
                Just x -> pure x
    case res of
        Right (nId, L.Response msgT msg) -> call nId (fromMsgType msgT) msg
        Right (nId, L.Update sub msg)    -> call nId (fromSubType sub) msg
        Left ()                          -> throwM CRETimeout
  where
    withHandler x =
        catch x $ \(e :: CliRecvExcInternal) -> do
            logWarning $ fromString $ "Could not receive: " <> show e <> ", retrying"
            case e of
                CRENoCallback n -> throwM $ CREUnexpected $ "No callback for " <> show n
                -- we ignore messages related to malformed input from
                -- the network (just warn)
                _               -> withHandler x

    withTimeout :: (STM () -> m x) -> m x
    withTimeout action
        | timeout < 0 = action retry
        | otherwise = do
              timeoutVar <- liftIO newEmptyTMVarIO
              let timer = do
                      liftIO $ threadDelay timeout
                      atomically $ putTMVar timeoutVar ()
              withAsync timer $ \_async -> action (readTMVar timeoutVar)

cliRecvOne ::
       forall k t d m. SendConstraint k t d m
    => ClientEnv t
    -> Int
    -> m (NodeId t, d)
cliRecvOne btq timeout =
    cliRecv @t @k @d
        btq
        timeout
        [ handlerDecoded $ \(nId :: NodeId t) ->
              either (\e -> throwM $ CREMalformed $ "cliRecvOne parse error " <> show e)
                     (pure . (nId,))
        ]

-- | Receive a response.
cliRecvResp ::
       forall t d m. SendConstraint MsgK t d m
    => ClientEnv t
    -> Int
    -> m (NodeId t, d)
cliRecvResp = cliRecvOne @MsgK @t @d

-- | Receive an update.
cliRecvUpdate ::
       forall t d m. SendConstraint SubK t d m
    => ClientEnv t
    -> Int
    -> m (NodeId t, d)
cliRecvUpdate = cliRecvOne @SubK @t @d

----------------------------------------------------------------------------
-- Launching
----------------------------------------------------------------------------

-- | Launch client broker on the background.
withClient :: (MonadUnliftIO m, NetworkingCli t m) => m a -> m a
withClient = withAsync runClient . const

-- | Launch server and client brokers on the background.
withServer :: (MonadUnliftIO m, NetworkingServ t m, NetworkingCli t m) => m a -> m a
withServer = withAsync runServer . const . withAsync runClient . const

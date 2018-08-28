{-# LANGUAGE PolyKinds #-}

-- | Simple notify-wait capabilities.
module Dscp.Util.Concurrent.NotifyWait
    ( Notifier
    , Waiter
    , notify
    , notifyCompleted
    , notifyError
    , finallyNotify
    , wait
    , newWaitPair
    ) where

import Control.Concurrent.STM (retry)
import Control.Exception (BlockedIndefinitelyOnSTM (..))
import Data.Reflection (Reifies (..))
import qualified Data.Text.Buildable
import Fmt ((+|), (|+))
import Loot.Log (MonadLogging)
import Time (Second, Time)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Exception as UIO

import Dscp.Util.TimeLimit

-- | Contains action which notifies about the event.
newtype Notifier (desc :: k) = Notifier (Either SomeException () -> IO ())

-- | Send notification about the event.
--
-- You /must/ make sure this will be called.
-- Operation is idempotent.
notify
    :: forall desc m. MonadIO m
    => Notifier desc -> Either SomeException () -> m ()
notify (Notifier doNotify) res = liftIO $ doNotify res

-- | Shortcut for 'notify' for successfull completion.
notifyCompleted
    :: forall desc m. MonadIO m
    => Notifier desc -> m ()
notifyCompleted notifier = notify notifier (Right ())

-- | Shortcut for 'notify' for failure case.
notifyError
    :: forall desc m. MonadIO m
    => Notifier desc -> SomeException -> m ()
notifyError notifier err = notify notifier (Left err)

-- | Notify waiter about given action termination, be it successful completion
-- or termination caused by exception. Any exceptions are rethrown (in this
-- thread).
-- Note that this function is exception-safe only in absence of async exceptions.
finallyNotify :: MonadUnliftIO m => m a -> Notifier desc -> m a
finallyNotify action notifier =
    (action <* notifyCompleted notifier)
        `UIO.catchAny` \e -> notifyError notifier e >> UIO.throwIO e

-- | Indicates a bug when there was no notification about the event
-- and it will never actually happen for sure.
newtype NotificationNeverFiredError = NotificationNeverFiredError Text
    deriving (Show)

instance Exception NotificationNeverFiredError

instance Buildable NotificationNeverFiredError where
    build (NotificationNeverFiredError eventDesc) =
        "Notification about event " +| eventDesc |+ " never fired"

-- | Contains action which blocks until the event happens.
newtype Waiter (desc :: k) = Waiter (IO (Either SomeException ()))

-- | Blocks until the event happens.
-- Can be used several times; consequent calls will terminate immediately.
wait :: forall desc text m.
    (MonadUnliftIO m, MonadLogging m, Reifies desc text, ToText text)
    => Waiter desc -> m ()
wait (Waiter doWait) = do
    let doWait' = liftIO doWait
          `UIO.catch` \BlockedIndefinitelyOnSTM ->
                     UIO.throwIO $ NotificationNeverFiredError eventDesc
    res <- logWarningWaitInf
              (1 :: Time Second) ("waiting for event '" <> eventDesc <> "'")
              doWait'
    either UIO.throwIO pure res
  where
    eventDesc = toText $ reflect (Proxy @desc)

-- | Creates a wait-notify pair.
newWaitPair :: MonadIO m => m (Notifier d, Waiter d)
newWaitPair = do
    eventResult <- newTVarIO (Nothing :: Maybe (Either SomeException ()))
    return
        ( Notifier $ \res ->
            atomically $ writeTVar eventResult (Just res)
        , Waiter $ atomically $
            readTVar eventResult >>= maybe retry return
        )

{-# LANGUAGE PolyKinds #-}

-- | Simple notify-wait capabilities.
module Dscp.Util.Concurrent.NotifyWait
    ( Notifier
    , Waiter
    , notify
    , wait
    , newWaitPair
    ) where

import Control.Concurrent.STM (check)
import Control.Exception (BlockedIndefinitelyOnSTM (..))
import Data.Reflection (Reifies (..))
import qualified Data.Text.Buildable
import Fmt ((+|), (|+))
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Exception as UIO

-- | Contains action which notifies about the event.
newtype Notifier (desc :: k) = Notifier (IO ())

-- | Send notification about the event.
--
-- You /have to/ make sure this will be called.
-- Operation is idempotent.
notify :: forall desc m. MonadIO m => Notifier desc -> m ()
notify (Notifier action) = liftIO action

-- | Indicates a bug when there was no notification about the event
-- and it will never actually happen for sure.
newtype NotificationNeverFiredError = NotificationNeverFiredError Text
    deriving (Show)

instance Exception NotificationNeverFiredError

instance Buildable NotificationNeverFiredError where
    build (NotificationNeverFiredError eventDesc) =
        "Notification about event " +| eventDesc |+ " never fired"

-- | Contains action which blocks until the event happens.
newtype Waiter (desc :: k) = Waiter (IO ())

-- | Blocks until the event happens.
-- Can be used several times; consequent calls will terminate immediately.
wait :: forall desc text m.
    (MonadUnliftIO m, Reifies desc text, ToText text)
    => Waiter desc -> m ()
wait (Waiter action) =
    liftIO action
        `UIO.catch` \BlockedIndefinitelyOnSTM ->
                     UIO.throwIO $ NotificationNeverFiredError eventDesc
  where
    eventDesc = toText $ reflect (Proxy @desc)

-- | Creates a wait-notify pair.
newWaitPair :: MonadIO m => m (Notifier d, Waiter d)
newWaitPair = do
    notified <- newTVarIO False
    return
        ( Notifier $ atomically $ writeTVar notified True
        , Waiter $ atomically $ readTVar notified >>= check
        )

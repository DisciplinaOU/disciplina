module Dscp.Util.Timing
       ( countingTime
       , foreverAlive
       , periodically
       ) where

import qualified Control.Concurrent as C
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fmt ((+|), (+||), (|+), (||+))
import Loot.Log (MonadLogging, logError)
import Time (KnownRat, KnownRatName, Second, Time, threadDelay, toNum)
import UnliftIO (MonadUnliftIO)

import Dscp.Util.TimeLimit (logWarningWaitOnce)

-- | Evaluate how much time did action take.
countingTime :: MonadIO m => m a -> m (Double, a)
countingTime m = do
    t <- liftIO $ getPOSIXTime
    a <- m
    t' <- liftIO $ getPOSIXTime
    let diff :: Double
        diff = fromRational . toRational $ t' - t
    return (diff, a)

-- | Continuously execute the action, recovering from failures.
foreverAlive
    :: (MonadIO m, MonadCatch m, MonadLogging m,
        KnownRat unit, KnownRatName unit)
    => Text -> Time unit -> m () -> m a
foreverAlive name recovery action =
    forever action `catchAny` \e -> do
        printNecrologue e
        threadDelay recovery
        foreverAlive name recovery action
  where
    printNecrologue e =
        logError $ name |+ " died (" +|| e ||+ "); \
                   \ressurecting in " +|| recovery ||+ ""

-- | Execute an action periodically till the end of node's days.
periodically
    :: (MonadUnliftIO m, MonadCatch m, MonadLogging m, KnownRat unit)
    => Text -> Time unit -> m () -> m a
periodically desc period action = loop
  where
    periodSec = toNum @Second period
    loop = do
        start <- liftIO getPOSIXTime
        safeAction
        end <- liftIO getPOSIXTime
        let remaining = periodSec - (end - start)
        liftIO $ C.threadDelay (round $ remaining * 1000000)
        loop
    safeAction =
        logWarningWaitOnce period desc $
        action `catchAny` reportFailure
    reportFailure e =
        logError $ "Periodic action '" +| desc |+ "' failed: " +|| e ||+ ""

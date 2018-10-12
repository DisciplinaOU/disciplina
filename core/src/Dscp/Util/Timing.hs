module Dscp.Util.Timing
       ( countingTime
       , foreverAlive
       , notFasterThan
       ) where

import qualified Control.Concurrent as C
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fmt ((+||), (|+), (||+))
import Loot.Log (MonadLogging, logError)
import Time (KnownRat, KnownRatName, Second, Time, threadDelay, toNum)

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

-- | Execute an action, finishing not faster than in given amount of time.
-- Useful in pair with 'foreverAlive' to get periodically invoked actions.
notFasterThan
    :: (MonadIO m, KnownRat unit)
    => Time unit -> m () -> m ()
notFasterThan minTime action = do
    (takenTime, ()) <- countingTime action
    let remaining = toNum @Second minTime - takenTime
    liftIO $ C.threadDelay (round $ remaining * 1000000)

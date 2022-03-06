module Dscp.Util.Timing
       ( countingTime
       , notFasterThan
       , recoverAll
       , retryOnSpot
       , constDelay
       , expBackoff
       , capDelay
       ) where

import Universum
import qualified Control.Concurrent as C
import Control.Exception.Safe (Handler (..))
import qualified Control.Retry as R
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fmt (ordinalF, (+|), (+||), (|+), (||+))
import Loot.Log (MonadLogging, logError)
import Time (KnownDivRat, Microsecond, Second, Time, toNum)

-- | Evaluate how much time did action take.
countingTime :: MonadIO m => m a -> m (Double, a)
countingTime m = do
    t <- liftIO $ getPOSIXTime
    a <- m
    t' <- liftIO $ getPOSIXTime
    let diff :: Double
        diff = fromRational . toRational $ t' - t
    return (diff, a)

-- | Execute an action, finishing not faster than in given amount of time.
-- Useful in pair with 'foreverAlive' to get periodically invoked actions.
notFasterThan
    :: (MonadIO m, KnownDivRat unit Second)
    => Time unit -> m () -> m ()
notFasterThan minTime action = do
    (takenTime, ()) <- countingTime action
    let remaining = toNum @Second minTime - takenTime
    liftIO $ C.threadDelay (round $ remaining * 1000000)

-- | Like 'R.recoverAll' retries on any synchronous error, and also does logging.
recoverAll
    :: forall m a. (MonadIO m, MonadMask m, MonadLogging m)
    => Text -> R.RetryPolicyM m -> m a -> m a
recoverAll name p action = R.recovering p handlers (const action)
  where
    handlers =
        R.skipAsyncExceptions <>
        [\status -> Handler $ \(SomeException e) -> reportFailure status e $> True]
    reportFailure :: Exception e => R.RetryStatus -> e -> m ()
    reportFailure st e =
        logError $ name |+ " failed [" +| ordinalF (R.rsIterNumber st + 1)
                        |+ " time / retrying for " +| (R.rsCumulativeDelay st `div` 1000000)
                        |+ "s in total]: " +|| e ||+ ""

-- | No delay before next retry.
retryOnSpot :: R.RetryPolicy
retryOnSpot = mempty

-- | Like 'R.constantDelay', but with a prettier way to specify time.
constDelay :: KnownDivRat unit Microsecond => Time unit -> R.RetryPolicy
constDelay = R.constantDelay . toNum @Microsecond

-- | Like 'R.exponentialBackoff', but with a prettier way to specify time.
expBackoff :: KnownDivRat unit Microsecond => Time unit -> R.RetryPolicy
expBackoff = R.exponentialBackoff . toNum @Microsecond

-- | Like 'R.capDelay', but with a prettier way to specify time.
capDelay
    :: (Monad m, KnownDivRat unit Microsecond)
    => Time unit -> R.RetryPolicyM m -> R.RetryPolicyM m
capDelay = R.capDelay . toNum @Microsecond

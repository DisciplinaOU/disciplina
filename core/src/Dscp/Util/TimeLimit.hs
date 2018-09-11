-- | Actions to handle long waits.
--
-- Copied @shersh's (D. Kovanikov) work from you know where.

module Dscp.Util.TimeLimit
       (
         -- * Log warning when action takes too much time
         CanLogInParallel
       , WaitingDelta (..)
       , logWarningLongAction
       , logWarningWaitOnce
       , logWarningWaitLinear
       , logWarningWaitInf
       ) where

import Fmt ((+|), (+||), (|+), (||+))
import Loot.Log (MonadLogging, logWarning)
import Time (KnownRat, Microsecond, Second, Time, floorUnit, threadDelay, toUnit)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (withAsyncWithUnmask)

-- | Data type to represent waiting strategy for printing warnings
-- if action take too much time.
data WaitingDelta
    = WaitOnce      (Time Second)              -- ^ wait s seconds and stop execution
    | WaitLinear    (Time Second)              -- ^ wait s, s * 2, s * 3  , s * 4  , ...      seconds
    | WaitGeometric (Time Microsecond) Double  -- ^ wait m, m * q, m * q^2, m * q^3, ... microseconds
    deriving (Show)

-- | Constraint for something that can be logged in parallel with other action.
type CanLogInParallel m = (MonadUnliftIO m, MonadLogging m)


-- | Run action and print warning if it takes more time than expected.
logWarningLongAction
    :: forall m a.
       CanLogInParallel m
    => WaitingDelta -> Text -> m a -> m a
logWarningLongAction delta actionTag action =
    -- Previous implementation was
    --
    --   bracket (fork $ waitAndWarn delta) killThread (const action)
    --
    -- but this has a subtle problem: 'killThread' can be interrupted even
    -- when exceptions are masked, so it's possible that the forked thread is
    -- left running, polluting the logs with misinformation.
    --
    -- 'withAsync' is assumed to take care of this, and indeed it does for
    -- 'Production's implementation, which uses the definition from the async
    -- package: 'uninterruptibleCancel' is used to kill the thread.
    --
    -- thinking even more about it, unmasking auxilary thread is crucial if
    -- this function is going to be called under 'mask'.
    withAsyncWithUnmask (\unmask -> unmask $ waitAndWarn delta) (const action)
  where
    printWarning t =
        logWarning $ "Action `" +| actionTag |+ "` took more than " +|| t ||+ ""

    waitAndWarn (WaitOnce      s  ) = threadDelay s >> printWarning s
    waitAndWarn (WaitLinear    s  ) =
        let waitLoop acc = do
                threadDelay s
                printWarning acc
                waitLoop (acc + s)
        in waitLoop s
    waitAndWarn (WaitGeometric s q) =
        let waitLoop !acc !t = do
                threadDelay t
                let newAcc  = acc + t
                let newT    = realToFrac $ realToFrac t * q
                printWarning (floorUnit @Second $ toUnit newAcc)
                waitLoop newAcc newT
        in waitLoop 0 s

{- Helper functions to avoid dealing with data type -}

-- | Specialization of 'logWarningLongAction' with 'WaitOnce'.
logWarningWaitOnce :: (CanLogInParallel m, KnownRat u) => Time u -> Text -> m a -> m a
logWarningWaitOnce = logWarningLongAction . WaitOnce . toUnit

-- | Specialization of 'logWarningLongAction' with 'WaitLinear'.
logWarningWaitLinear :: (CanLogInParallel m, KnownRat u) => Time u -> Text -> m a -> m a
logWarningWaitLinear = logWarningLongAction . WaitLinear . toUnit

-- | Specialization of 'logWarningLongAction' with 'WaitGeometric'
-- with parameter @1.3@. Accepts 'Second'.
logWarningWaitInf :: (CanLogInParallel m, KnownRat u) => Time u -> Text -> m a -> m a
logWarningWaitInf = logWarningLongAction . (`WaitGeometric` 1.7) . toUnit

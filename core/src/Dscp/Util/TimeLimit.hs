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

         -- * Action boundExecution
       , boundExecution
       , boundExecution_
       ) where

import Universum
import Fmt ((+|), (+||), (|+), (||+))
import Loot.Log (MonadLogging, logError, logWarning)
import Time (KnownDivRat, KnownUnitName, Microsecond, Second, Time, floorUnit, threadDelay, toUnit, toNum, time)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (async, asyncWithUnmask, cancel, uninterruptibleCancel, waitEither,
                       withAsyncWithUnmask)
import qualified UnliftIO.Exception as UIO

----------------------------------------------------------------------------
-- Warn
----------------------------------------------------------------------------

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
                waitLoop (acc <> s)
        in waitLoop s
    waitAndWarn (WaitGeometric s q) =
        let waitLoop !acc !t = do
                threadDelay t
                let newAcc  = acc <> t
                let newT    = time @Microsecond $ realToFrac $ toNum @Microsecond @Double t * q
                printWarning (floorUnit @Second $ toUnit newAcc)
                waitLoop newAcc newT
        in waitLoop (time @Microsecond 0) s

{- Helper functions to avoid dealing with data type -}

-- | Specialization of 'logWarningLongAction' with 'WaitOnce'.
logWarningWaitOnce :: (CanLogInParallel m, KnownDivRat u Second) => Time u -> Text -> m a -> m a
logWarningWaitOnce = logWarningLongAction . WaitOnce . toUnit

-- | Specialization of 'logWarningLongAction' with 'WaitLinear'.
logWarningWaitLinear :: (CanLogInParallel m, KnownDivRat u Second) => Time u -> Text -> m a -> m a
logWarningWaitLinear = logWarningLongAction . WaitLinear . toUnit

-- | Specialization of 'logWarningLongAction' with 'WaitGeometric'
-- with parameter @1.3@. Accepts 'Second'.
logWarningWaitInf :: (CanLogInParallel m, KnownDivRat u Microsecond) => Time u -> Text -> m a -> m a
logWarningWaitInf = logWarningLongAction . (`WaitGeometric` 1.7) . toUnit

----------------------------------------------------------------------------
-- Kill
----------------------------------------------------------------------------

-- | Execute the given action no longer than for the given amount of time.
--
-- Unlike in 'timeout', the action will be killed asynchronously - this may help when
-- you are not sure whether will it die smoothly (e.g. because it is uninterruptibly
-- blocked on some operation or makes FFI calls).
boundExecution
    :: (MonadUnliftIO m, MonadLogging m, KnownDivRat u Microsecond, KnownUnitName u)
    => Time u -> Text -> m a -> m (Maybe a)
boundExecution time' desc action =
    UIO.mask $ \restore -> do
        -- further we cannot use 'restore' since 'boundExecution' call can happen
        -- under masked state (e.g. in resources cleanup), and this state would be inherited.
        timerAsync <- asyncWithUnmask $ \unmask -> unmask (threadDelay time')
        actionAsync <- asyncWithUnmask $ \unmask -> unmask action

        res <- restore $ waitEither timerAsync actionAsync
        case res of
            Left () -> do
                void . async $ cancel actionAsync
                restore logTimeouted
                return Nothing
            Right a -> do
                uninterruptibleCancel timerAsync
                return (Just a)
  where
    logTimeouted =
        logError $ "Action " +|| desc ||+ " timeouted (after " +|| time' ||+ ")"

boundExecution_
    :: (MonadUnliftIO m, MonadLogging m, KnownDivRat u Microsecond, KnownUnitName u)
    => Time u -> Text -> m a -> m ()
boundExecution_ = void ... boundExecution

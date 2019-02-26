-- | Time primitives with emulation support.

module Dscp.Util.Time
    ( infiniteFuture
    , timeDiffNonNegative

      -- * Cababilities
    , TimeActions
    , TestTimeActions
    , HasTime
    , HasTestTime
    , getCurTime
    , getCurTimeMcs
    , sleep
    , rewindTime

      -- * Implementations
    , realTimeActions
    , mkTestTimeActions
    ) where

import qualified Control.Concurrent.STM as STM
import Control.Exception (BlockedIndefinitelyOnSTM (..))
import Control.Exception.Safe (handle)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Loot.Base.HasLens (HasCtx, HasLens', lensOf)
import Time (KnownRat, Second, Time, Timestamp (..), fromUnixTime, threadDelay, timeAdd, timeDiff,
             toUnit)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Moment of time which will never happen, in theory.
infiniteFuture :: Timestamp
infiniteFuture = fromUnixTime @Int 4260211200  -- 1 Jan 2105

-- | Substruct timestamps, returning 0 if result is negative.
timeDiffNonNegative :: Timestamp -> Timestamp -> Time Second
timeDiffNonNegative t1 t2 =
    case timeDiff t1 t2 of
        (GT, d) -> d
        _       -> 0

----------------------------------------------------------------------------
-- Capabilities
----------------------------------------------------------------------------

-- | Basic timing actions.
data TimeActions = TimeActions
    { taGetCurrent :: IO Timestamp
    , taSleep      :: Time Second -> IO ()
    }

-- | Additional timing capabilities for tests.
data TestTimeActions = TestTimeActions
    { taRewind :: Time Second -> IO ()
    }

type HasTime ctx m =
    ( MonadIO m
    , HasCtx ctx m '[TimeActions]
    )

type HasTestTime ctx m =
    ( HasTime ctx m
    , HasLens' ctx TestTimeActions
    )

-- | Get current time.
getCurTime :: HasTime ctx m => m Timestamp
getCurTime = do
    TimeActions{..} <- view $ lensOf @TimeActions
    liftIO taGetCurrent

-- | Get current time, in microseconds.
getCurTimeMcs :: HasTime ctx m => m Word64
getCurTimeMcs = getCurTime <&> \(Timestamp t) -> floor (t * 1000000)

-- | Analogy to 'threadDelay'.
sleep :: (KnownRat unit, HasTime ctx m) => Time unit -> m ()
sleep duration = do
    TimeActions{..} <- view $ lensOf @TimeActions
    liftIO $ taSleep (toUnit duration)

-- | Instantly increase current time by given amount.
rewindTime :: (KnownRat unit, HasTestTime ctx m) => Time unit -> m ()
rewindTime by = do
    TestTimeActions{..} <- view $ lensOf @TestTimeActions
    liftIO $ taRewind (toUnit by)

----------------------------------------------------------------------------
-- Implementations
----------------------------------------------------------------------------

-- | Makes actions refering CPU time.
realTimeActions :: TimeActions
realTimeActions =
    TimeActions
    { taGetCurrent = fromUnixTime <$> getPOSIXTime
    , taSleep = threadDelay
    }

-- | Make actions refering emulated, manually controlled notion of time.
-- This is all naive for now as soon as it's non-deterministic, especially
-- in presense of several threads performing 'sleep'.
mkTestTimeActions :: MonadIO m => m (TimeActions, TestTimeActions)
mkTestTimeActions = do
    timeVar <- newTVarIO (Timestamp 0)
    return
        ( TimeActions
          { taGetCurrent =
              atomically $ readTVar timeVar

          , taSleep = \duration -> do
              start <- atomically $ readTVar timeVar
              let end = duration `timeAdd` start
              handleSleepHangs $
                  atomically $ readTVar timeVar >>= STM.check . (>= end)
          }
        , TestTimeActions
          { taRewind = \by ->
              atomically $ modifyTVar' timeVar (timeAdd by)
          }
        )
  where
    handleSleepHangs =
        handle $ \BlockedIndefinitelyOnSTM ->
            error "And this thread has never awaken again..."

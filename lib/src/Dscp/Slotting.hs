-- | Minimalistic slotting. Currently we only need to get uniform
-- slots numbers so that several nodes can issue blocks one after
-- another.

module Dscp.Slotting
       ( getCurrentSlot
       , waitUntilNextSlot
       ) where

import Control.Concurrent (threadDelay)
import Data.Time.Clock.POSIX (getPOSIXTime)

newtype SlotId = SlotId Integer deriving (Eq, Ord, Show, Generic)

-- TODO pass it instead of fixing.
-- In microseconds.
slotLength :: Integer
slotLength = 30000000 -- 30 sec

getTimeMcs :: MonadIO m => m Integer
getTimeMcs = floor . (*1000000) . toRational <$> liftIO getPOSIXTime

-- Yes. This is enough.
getCurrentSlot :: MonadIO m => m SlotId
getCurrentSlot = do
    curTime <- getTimeMcs
    pure $ SlotId $ curTime `div` slotLength

waitUntilNextSlot :: MonadIO m => m SlotId
waitUntilNextSlot = do
    curTime <- getTimeMcs
    liftIO $ threadDelay $ fromIntegral $ slotLength - (curTime `mod` slotLength)
    pure $ SlotId $ 1 + (curTime `div` slotLength)

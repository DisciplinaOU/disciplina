-- | Super-basic slotting.

module Dscp.Core.Slotting
       ( SlotId (..)
       , getCurrentSlot
       , getSlotSince
       , waitUntilNextSlot
       ) where

import Control.Concurrent (threadDelay)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Dscp.Core.Config
import Dscp.Core.Foundation (SlotId (..))

-- In microseconds.
slotLength :: HasCoreConfig => Word64
slotLength = (*1000) $ unSlotDuration $ giveL @CoreConfig

getTimeMcs :: MonadIO m => m Word64
getTimeMcs = floor . (*1000000) . toRational <$> liftIO getPOSIXTime

-- Yes. This is enough.
getCurrentSlot :: (HasCoreConfig, MonadIO m) => m SlotId
getCurrentSlot = do
    curTime <- getTimeMcs
    pure $ SlotId $ curTime `div` slotLength

-- Time of slot start, in microseconds
getSlotSince :: HasCoreConfig => SlotId -> Word64
getSlotSince (SlotId i) = i * slotLength

waitUntilNextSlot :: (HasCoreConfig, MonadIO m) => m SlotId
waitUntilNextSlot = do
    curTime <- getTimeMcs
    liftIO $ threadDelay $ fromIntegral $ slotLength - (curTime `mod` slotLength)
    pure $ SlotId $ 1 + (curTime `div` slotLength)

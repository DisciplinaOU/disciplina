-- | Super-basic slotting.

module Dscp.Core.Slotting
       ( SlotId (..)
       , getCurrentSlot
       , waitUntilNextSlot
       ) where

import Control.Concurrent (threadDelay)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Dscp.Core.Foundation (SlotId (..))

-- TODO pass it instead of fixing.
-- In microseconds.
slotLength :: Word64
slotLength = 20000000 -- 20 sec

getTimeMcs :: MonadIO m => m Word64
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

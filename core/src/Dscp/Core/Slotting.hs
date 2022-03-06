-- | Super-basic slotting.

module Dscp.Core.Slotting
       ( SlotId (..)
       , getCurrentSlot
       , getSlotSince
       , waitUntilNextSlot
       , rewindToNextSlot
       ) where

import Universum
import Time (Microsecond, time)

import Dscp.Core.Config
import Dscp.Core.Foundation (SlotId (..))
import Dscp.Util.Time

-- In microseconds.
slotLength :: HasCoreConfig => Word64
slotLength = (*1000) $ unSlotDuration $ giveL @CoreConfig

-- Yes. This is enough.
getCurrentSlot :: (HasCoreConfig, HasTime ctx m) => m SlotId
getCurrentSlot = do
    curTime <- getCurTimeMcs
    pure $ SlotId $ curTime `div` slotLength

-- Time of slot start, in microseconds
getSlotSince :: HasCoreConfig => SlotId -> Word64
getSlotSince (SlotId i) = i * slotLength

-- | How much time till next slot start, in microseconds.
remainingTillNextSlot :: (HasCoreConfig, HasTime ctx m) => m Word64
remainingTillNextSlot = do
    curTime <- getCurTimeMcs
    return $ slotLength - (curTime `mod` slotLength)

waitUntilNextSlot :: (HasCoreConfig, HasTime ctx m) => m SlotId
waitUntilNextSlot = do
    sleep . time @Microsecond . realToFrac =<< remainingTillNextSlot
    getCurrentSlot

-- | Rewind emulated time to the start of the next slot.
-- Returns id of just arrived slot.
rewindToNextSlot :: (HasCoreConfig, HasTestTime ctx m) => m SlotId
rewindToNextSlot = do
    rewindTime . time @Microsecond . realToFrac =<< remainingTillNextSlot
    getCurrentSlot

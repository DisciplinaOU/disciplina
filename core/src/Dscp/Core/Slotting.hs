-- | Super-basic slotting.

module Dscp.Core.Slotting
       ( SlotId (..)
       , getCurrentSlot
       , slotFromMcs
       , getSlotSince
       , waitUntilNextSlot
       , rewindToNextSlot
       ) where

import Time (Microsecond)

import Dscp.Core.Config
import Dscp.Core.Foundation (SlotId (..))
import Dscp.Util.Time

-- In microseconds.
slotLength :: HasCoreConfig => Word64
slotLength = (*1000) $ unSlotDuration $ giveL @CoreConfig

-- Yes. This is enough.
getCurrentSlot :: (HasCoreConfig, HasTime ctx m) => m SlotId
getCurrentSlot = slotFromMcs <$> getCurTimeMcs

-- | Get 'SlotId' from time in microseconds
slotFromMcs :: (HasCoreConfig) => Word64 -> SlotId
slotFromMcs time = SlotId $ time `div` slotLength

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
    sleep @Microsecond . fromIntegral =<< remainingTillNextSlot
    getCurrentSlot

-- | Rewind emulated time to the start of the next slot.
-- Returns id of just arrived slot.
rewindToNextSlot :: (HasCoreConfig, HasTestTime ctx m) => m SlotId
rewindToNextSlot = do
    rewindTime @Microsecond . fromIntegral =<< remainingTillNextSlot
    getCurrentSlot

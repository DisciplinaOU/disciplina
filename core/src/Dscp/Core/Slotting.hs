-- | Super-basic slotting.

module Dscp.Core.Slotting
       ( -- * Basic operations
         SlotId (..)
       , getCurrentSlot
       , getSlotSince
       , waitUntilNextSlot
       , increaseCurrentSlotBy

         -- * Context
       , SlottingActions
       , realSlottingActions
       , mkTestSlottingActions
       ) where

import Control.Concurrent (threadDelay)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Type.Equality ((:~:) (..))
import Loot.Base.HasLens (HasCtx, lensOf)

import Dscp.Core.Config
import Dscp.Core.Foundation (SlotId (..))
import Dscp.Launcher.Mode
import Dscp.Util

-- In microseconds.
slotLength :: HasCoreConfig => Word64
slotLength = (*1000) $ unSlotDuration $ giveL @CoreConfig

data SlottingActions (e :: ModeEnv) = SlottingActions
    { saGetCurrentSlot
        :: HasCoreConfig
        => IO SlotId
    , saModifyCurrentSlot
        :: Refuted (e :~: 'RealMode) -> (SlotId -> SlotId) -> IO ()
    , saWaitUntilNextSlot
        :: HasCoreConfig
        => Refuted (e :~: 'TestMode) -> IO SlotId
    }

type HasSlotting e ctx m =
    ( MonadIO m
    , HasCtx ctx m '[SlottingActions e]
    , HasCoreConfig
    )

getCurrentSlot
    :: forall e ctx m.
       HasSlotting e ctx m
    => m SlotId
getCurrentSlot = do
    SlottingActions{..} <- view $ lensOf @(SlottingActions e)
    liftIO saGetCurrentSlot

modifyCurrentSlot
    :: forall e ctx m.
       (HasSlotting e ctx m, e ~ 'TestMode)
    => (SlotId -> SlotId) -> m ()
modifyCurrentSlot f = do
    SlottingActions{..} <- view $ lensOf @(SlottingActions e)
    liftIO $ saModifyCurrentSlot (\case) f

increaseCurrentSlotBy
    :: forall e ctx m.
       (HasSlotting e ctx m, e ~ 'TestMode)
    => Int -> m ()
increaseCurrentSlotBy n = modifyCurrentSlot (+ fromIntegral n)

-- Time of slot start, in microseconds
getSlotSince :: HasCoreConfig => SlotId -> Word64
getSlotSince (SlotId i) = i * slotLength

waitUntilNextSlot
    :: forall e ctx m.
       (HasSlotting e ctx m, e ~ 'RealMode)
    => m SlotId
waitUntilNextSlot = do
    SlottingActions{..} <- view $ lensOf @(SlottingActions e)
    liftIO $ saWaitUntilNextSlot (\case)

---------------------------------------------------------------------
-- Slotting implementations
---------------------------------------------------------------------

getTimeMcs :: MonadIO m => m Word64
getTimeMcs = floor . (*1000000) . toRational <$> liftIO getPOSIXTime

realSlottingActions :: SlottingActions 'RealMode
realSlottingActions =
    SlottingActions
    { saGetCurrentSlot = do
        -- Yes. This is enough.
        curTime <- getTimeMcs
        pure $ SlotId $ curTime `div` slotLength

    , saModifyCurrentSlot = \thisIsTest ->
        absurd $ thisIsTest Refl

    , saWaitUntilNextSlot = \_thisIsReal -> do
        curTime <- getTimeMcs
        liftIO $ threadDelay $ fromIntegral $ slotLength - (curTime `mod` slotLength)
        pure $ SlotId $ 1 + (curTime `div` slotLength)
    }

mkTestSlottingActions :: MonadIO m => m (SlottingActions 'TestMode)
mkTestSlottingActions = do
    slotBox <- newTVarIO @_ @SlotId 0
    return $ SlottingActions
        { saGetCurrentSlot = readTVarIO slotBox
        , saModifyCurrentSlot = \_ f -> atomically $ modifyTVar' slotBox f
        , saWaitUntilNextSlot = \thisIsReal -> absurd $ thisIsReal Refl
        }

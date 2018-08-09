
module Dscp.Witness.SDLock
    ( reading
    , writing
    , new
    ) where

import qualified Control.Concurrent.ReadWriteLock as Lock
import qualified UnliftIO

import Loot.Base.HasLens (HasLens (..))

import Dscp.Witness.Launcher.Mode (SDLock (..), WitnessWorkMode)

new :: MonadIO m => m SDLock
new = liftIO $ SDLock <$> Lock.new

reading :: WitnessWorkMode ctx m => m a -> m a
reading action = do
    SDLock lock <- view (lensOf @SDLock)
    UnliftIO.withRunInIO $ \unliftIO -> do
        Lock.withRead lock $ do
            unliftIO action

writing :: WitnessWorkMode ctx m => m a -> m a
writing action = do
    SDLock lock <- view (lensOf @SDLock)
    UnliftIO.withRunInIO $ \unliftIO -> do
        Lock.withWrite lock $ do
            unliftIO action

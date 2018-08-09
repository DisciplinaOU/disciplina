
module Dscp.Witness.SDLock
    ( readingSDLock
    , writingSDLock
    , newSDLock
    ) where

import qualified Control.Concurrent.ReadWriteLock as Lock
import qualified UnliftIO

import Loot.Base.HasLens (HasLens (..), HasLens')

import Dscp.Witness.Launcher.Mode (SDLock (..))

newSDLock :: MonadIO m => m SDLock
newSDLock = liftIO $ SDLock <$> Lock.new

readingSDLock
    ::  ( UnliftIO.MonadUnliftIO m
        , MonadReader ctx m
        , HasLens' ctx SDLock
        )
    => m a
    -> m a
readingSDLock action = do
    SDLock lock <- view (lensOf @SDLock)
    UnliftIO.withRunInIO $ \unliftIO ->
        Lock.withRead lock $ do
            unliftIO action

writingSDLock
    ::  ( UnliftIO.MonadUnliftIO m
        , MonadReader ctx m
        , HasLens' ctx SDLock
        )
    => m a
    -> m a
writingSDLock action = do
    SDLock lock <- view (lensOf @SDLock)
    UnliftIO.withRunInIO $ \unliftIO ->
        Lock.withWrite lock $ do
            unliftIO action

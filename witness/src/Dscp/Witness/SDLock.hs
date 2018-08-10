
module Dscp.Witness.SDLock
    ( SDLock
    , readingSDLock, readingSDLockOf
    , writingSDLock, writingSDLockOf
    , newSDLock
    ) where

import qualified Control.Concurrent.ReadWriteLock as RawLock
import qualified UnliftIO

import Loot.Base.HasLens (HasLens (..), HasLens')

newtype SDLock = SDLock RawLock.RWLock

newSDLock :: MonadIO m => m SDLock
newSDLock = liftIO $ SDLock <$> RawLock.new

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
        RawLock.withRead lock $ do
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
        RawLock.withWrite lock $ do
            unliftIO action

readingSDLockOf
    :: UnliftIO.MonadUnliftIO m
    => SDLock
    -> m a
    -> m a
readingSDLockOf (SDLock lock) action = do
    UnliftIO.withRunInIO $ \unliftIO ->
        RawLock.withRead lock $ do
            unliftIO action

writingSDLockOf
    :: UnliftIO.MonadUnliftIO m
    => SDLock
    -> m a
    -> m a
writingSDLockOf (SDLock lock) action = do
    UnliftIO.withRunInIO $ \unliftIO ->
        RawLock.withWrite lock $ do
            unliftIO action

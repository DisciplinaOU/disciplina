
module Dscp.Witness.SDLock
    ( SDLock
    , WithinReadSDLock, WithinWriteSDLock
    , readingSDLock, readingSDLockOf
    , writingSDLock, writingSDLockOf
    , newSDLock
    ) where

import qualified Control.Concurrent.ReadWriteLock as RawLock
import Data.Reflection (Given, give)
import qualified UnliftIO

import Loot.Base.HasLens (HasLens (..), HasLens')

newtype SDLock = SDLock RawLock.RWLock

data ReadSDLockTag = ReadSDLockTag
-- | Add this constraint when action is just a part of critical section to be
-- protected by read lock.
-- You can not take nested write + read locks, so this is handy.
type WithinReadSDLock = Given ReadSDLockTag

data WriteSDLockTag = WriteSDLockTag
-- | Add this constraint when action is just a part of critical section to be
-- protected by write lock.
-- You can not take nested write locks, so this is handy.
type WithinWriteSDLock = (Given WriteSDLockTag, WithinReadSDLock)

newSDLock :: MonadIO m => m SDLock
newSDLock = liftIO $ SDLock <$> RawLock.new

readingSDLock
    ::  ( UnliftIO.MonadUnliftIO m
        , MonadReader ctx m
        , HasLens' ctx SDLock
        )
    => (WithinReadSDLock => m a)
    -> m a
readingSDLock action = do
    lock <- view (lensOf @SDLock)
    readingSDLockOf lock action

writingSDLock
    ::  ( UnliftIO.MonadUnliftIO m
        , MonadReader ctx m
        , HasLens' ctx SDLock
        )
    => (WithinWriteSDLock => m a)
    -> m a
writingSDLock action = do
    lock <- view (lensOf @SDLock)
    writingSDLockOf lock action

readingSDLockOf
    :: UnliftIO.MonadUnliftIO m
    => SDLock
    -> (WithinReadSDLock => m a)
    -> m a
readingSDLockOf (SDLock lock) action = do
    UnliftIO.withRunInIO $ \unliftIO ->
        RawLock.withRead lock $ do
            unliftIO (give ReadSDLockTag action)

writingSDLockOf
    :: UnliftIO.MonadUnliftIO m
    => SDLock
    -> (WithinWriteSDLock => m a)
    -> m a
writingSDLockOf (SDLock lock) action = do
    UnliftIO.withRunInIO $ \unliftIO ->
        RawLock.withWrite lock $ do
            unliftIO (give ReadSDLockTag $ give WriteSDLockTag action)

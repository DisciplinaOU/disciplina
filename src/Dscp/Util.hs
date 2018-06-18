
-- | Utilities

module Dscp.Util
       ( anyMapM
       , wrapRethrow
       , wrapRethrowIO
         -- * Re-exports
       , module Snowdrop.Util
       ) where

import Universum

import Snowdrop.Util

deriving instance Container (b a) => Container (OldestFirst b a)
deriving instance Container (b a) => Container (NewestFirst b a)

anyMapM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyMapM _ [] = return False
anyMapM f (a:as) = f a >>= \case
    True -> return True
    False -> anyMapM f as

-- | Converts a possible error, used for wrapping exceptions using given
-- constructor into ADT-sum of exceptions.
wrapRethrow
    :: (Exception e1, Exception e2, MonadCatch m)
    => (e1 -> e2) -> m a -> m a
wrapRethrow wrap action = catch action (throwM . wrap)

-- | Handy for wrapping exceptions provided by libraries.
wrapRethrowIO
    :: (Exception e1, Exception e2, MonadCatch m, MonadIO m)
    => (e1 -> e2) -> IO a -> m a
wrapRethrowIO wrap action = wrapRethrow wrap (liftIO action)

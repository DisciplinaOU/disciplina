
-- | Utilities

module Dscp.Util
       ( anyMapM
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

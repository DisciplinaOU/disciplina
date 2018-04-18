
-- | Utilities

module Disciplina.Util
       ( -- * Re-exports
         module Snowdrop.Util
       ) where

import Universum

import Snowdrop.Util

deriving instance Container (b a) => Container (OldestFirst b a)
deriving instance Container (b a) => Container (NewestFirst b a)

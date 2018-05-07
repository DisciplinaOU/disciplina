
-- | 'HashFunction' implementation for 'Hashable' typeclass.
-- Here for demonstration. Never use this implementation in actual
-- cryptographic purposes!

module Disciplina.Crypto.Hash.Hashable
       ( HashableFunc
       ) where

import Universum

import Data.Hashable (Hashable)
import qualified Data.Hashable as H

import Disciplina.Crypto.ByteArray (hashBytes)
import Disciplina.Crypto.Hash.Class (AbstractHash (..), HasAbstractHash (..), HashFunc (..))

-- | Make 'AbstractHash' itself 'Hashable'
deriving instance Hashable (HashResult hf) =>
    Hashable (AbstractHash hf a)

-- | Tag for choosing 'hash' function from 'Data.Hashable' for hashing
data HashableFunc

instance HashFunc HashableFunc where
    type HashResult HashableFunc = Int
    unsafeHashBytes = AbstractHash . hashBytes

-- | Hash every 'Hashable' type (don't bother here with 'Serialise' and stuff...)
instance Hashable a => HasAbstractHash HashableFunc a where
    unsafeAbstractHash = AbstractHash . H.hash

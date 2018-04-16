
-- | 'HashFunction' implementation for 'Hashable' typeclass

module Disciplina.Crypto.Hash.Hashable
       ( HashableFunc (..)
       ) where

import Universum

import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray as BA
import Data.Hashable (Hashable)
import qualified Data.Hashable as H
import System.IO.Unsafe (unsafeDupablePerformIO)

import Disciplina.Crypto.Hash.Class (AbstractHash (..), HashFunction (..))

-- | Tag for choosing 'hash' function from 'Data.Hashable' for hashing
data HashableFunc = HashableFunc

-- | A newtype wrapper for correctly picking up a 'Hashable' instance
newtype HashableBAccess a = HashableBAccess a
    deriving (Show, Eq, Ord, ByteArrayAccess)

-- | 'Hashable' instance for any instance of 'ByteArrayAccess'
instance ByteArrayAccess a => Hashable (HashableBAccess a) where
    hashWithSalt salt ba =
        let l = BA.length ba
        in unsafeDupablePerformIO $ BA.withByteArray ba $ \p ->
            H.hashPtrWithSalt p l salt

-- | 'HashFunction' instance which uses 'Hashable' for 'ByteArray's
instance HashFunction HashableFunc where
    type HashResult HashableFunc = Int
    unsafeAbstractHash = AbstractHash . H.hash . HashableBAccess

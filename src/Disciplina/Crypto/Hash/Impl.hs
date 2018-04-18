
-- | Specific hash function implementations used in Disciplina

module Disciplina.Crypto.Hash.Impl
       ( Hash
       , hash
       , unsafeHash
       ) where

import Crypto.Hash.Algorithms (Blake2sp_256)
import Data.ByteArray (ByteArrayAccess)

import Disciplina.Crypto.Hash.Class (AbstractHash (..), HashFunction (..), abstractHash)
import Disciplina.Crypto.Hash.Cryptonite (CryptoniteFunc (..))

type Hash a = AbstractHash (CryptoniteFunc Blake2sp_256) a

-- TODO: These functions work for any 'ByteArrayAccess' instance.
-- But datatypes which have no such instance must be serialized before hashing.
-- Serialization is expensive (at least because of memory allocation and copying),
-- and we'd like to avoid it for datatypes with 'ByteArrayAccess'.
-- At the same time, calling 'serialise' manually every time for hashing some
-- non-trivial types is terrible. How can we do 'serialise' inside 'hash' function
-- for complex types, and omit it for types with 'ByteArrayAccess'?
hash :: forall a. ByteArrayAccess a => a -> Hash a
hash = abstractHash

unsafeHash :: forall a b. ByteArrayAccess a => a -> Hash b
unsafeHash = unsafeAbstractHash

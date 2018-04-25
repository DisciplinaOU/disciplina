
-- | Actual implementations of abstract crypto primitives used in Disciplina.

module Disciplina.Crypto.Impl
       ( Hash
       , HasHash
       , hash
       , unsafeHash
       ) where

import Crypto.Hash.Algorithms (Blake2sp_256)

import Disciplina.Crypto.Hash.Class (AbstractHash (..), HasAbstractHash (..), abstractHash)
import Disciplina.Crypto.Hash.Cryptonite (CryptoniteFunc)

------------------------------------------------------
-- Hashing
------------------------------------------------------

-- | We choose `blake2sp-256`
type HashFunc = CryptoniteFunc Blake2sp_256

type HasHash a = HasAbstractHash HashFunc a
type Hash a = AbstractHash HashFunc a

hash :: forall a. HasHash a => a -> Hash a
hash = abstractHash

unsafeHash :: forall a b. HasHash a => a -> Hash b
unsafeHash = unsafeAbstractHash

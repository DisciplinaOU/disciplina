
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

hash :: forall a. ByteArrayAccess a => a -> Hash a
hash = abstractHash

unsafeHash :: forall a b. ByteArrayAccess a => a -> Hash b
unsafeHash = unsafeAbstractHash

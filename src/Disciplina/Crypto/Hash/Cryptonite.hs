
-- | 'HashFunction' instances for 'Cryptonite' hashes

module Disciplina.Crypto.Hash.Cryptonite
       ( CryptoniteFunc (..)
       ) where

import qualified Crypto.Hash as Crypto

import Disciplina.Crypto.Hash.Class (HashFunction (..))

data CryptoniteFunc algo = CryptoniteFunc algo

instance Crypto.HashAlgorithm algo => HashFunction (CryptoniteFunc algo) where
    type AbstractHash (CryptoniteFunc algo) a = Crypto.Digest algo
    unsafeAbstractHash = Crypto.hash

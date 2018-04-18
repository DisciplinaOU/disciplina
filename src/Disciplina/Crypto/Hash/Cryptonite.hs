
-- | 'HashFunction' instances for 'Cryptonite' hashes

module Disciplina.Crypto.Hash.Cryptonite
       ( CryptoniteFunc (..)
       ) where

import Universum

import qualified Crypto.Hash as Crypto

import Disciplina.Crypto.Hash.Class (AbstractHash (..), HashFunction (..))

data CryptoniteFunc algo = CryptoniteFunc algo

instance Crypto.HashAlgorithm algo => HashFunction (CryptoniteFunc algo) where
    type HashResult (CryptoniteFunc algo) = Crypto.Digest algo
    unsafeAbstractHash = AbstractHash . Crypto.hash

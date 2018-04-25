{-# LANGUAGE TypeApplications #-}

-- | 'HashFunction' instances for 'Cryptonite' hashes

module Disciplina.Crypto.Hash.Cryptonite
       ( CryptoniteFunc
       ) where

import Universum

import qualified Crypto.Hash as Crypto
import Data.ByteArray (ByteArrayAccess)

import Disciplina.Crypto.Hash.Class (AbstractHash (..), HasAbstractHash (..), HashFunc (..))

-- | Provide 'ByteArrayAccess' instance for all byte array based hashes.
deriving instance ByteArrayAccess (HashResult hf) =>
    ByteArrayAccess (AbstractHash hf a)

-- | Tag for choosing a particular hash algorithm from 'Crypto.Hash'
-- for hashing.
data CryptoniteFunc algo

instance Crypto.HashAlgorithm algo =>
         HashFunc (CryptoniteFunc algo) where
    type HashResult (CryptoniteFunc algo) = Crypto.Digest algo

-- | Hash anything with 'ByteArrayAccess'.
instance (Crypto.HashAlgorithm algo, ByteArrayAccess a) =>
         HasAbstractHash (CryptoniteFunc algo) a where
    unsafeAbstractHash = AbstractHash . Crypto.hash

-- | Also hash lazy bytestrings.
instance {-# OVERLAPPING #-} Crypto.HashAlgorithm algo =>
         HasAbstractHash (CryptoniteFunc algo) LByteString where
    unsafeAbstractHash = AbstractHash . Crypto.hashlazy

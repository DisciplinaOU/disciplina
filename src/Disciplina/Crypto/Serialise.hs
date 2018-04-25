{-# LANGUAGE TypeApplications #-}

-- | Instances for binary serialisation for 'Disciplina.Crypto' datatypes

module Disciplina.Crypto.Serialise () where

import Universum

import Codec.Serialise (Serialise (..), serialise)
import qualified Crypto.Hash as Crypto
import Data.ByteArray (convert)

import Disciplina.Crypto.Hash.Class (AbstractHash (..), HasAbstractHash (..), HashFunc (..))

---------------------------------------------------------------
-- Hashes
---------------------------------------------------------------

-- | 'Serialise' instance for 'Digest a', to make Cryptonite-based hashes
-- serialisable.
instance Crypto.HashAlgorithm algo => Serialise (Crypto.Digest algo) where
    decode = do
        bs <- decode
        maybe (fail "invalid hash representation") return $
            Crypto.digestFromByteString (bs :: ByteString)

    encode = encode @ByteString . convert

-- | 'Serialise' instance for every 'AbstractHash' with
-- serialisable 'HashResult'.
deriving instance Serialise (HashResult hf) =>
    Serialise (AbstractHash hf a)

-- | If 'LByteString' can be hashed with method `hf`, then every
-- 'Serialise' instance can.
instance {-# OVERLAPPABLE #-}
    (Serialise a, HashFunc hf, HasAbstractHash hf LByteString) =>
    HasAbstractHash hf a where
    unsafeAbstractHash = unsafeAbstractHash . serialise

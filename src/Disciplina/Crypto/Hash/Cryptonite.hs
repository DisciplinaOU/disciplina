{-# LANGUAGE TypeApplications #-}

-- | 'HashFunction' instances for 'Cryptonite' hashes

module Disciplina.Crypto.Hash.Cryptonite
       ( CryptoniteFunc (..)
       ) where

import Universum

import Codec.Serialise (Serialise (..))
import qualified Crypto.Hash as Crypto
import Data.ByteArray (convert)

import Disciplina.Crypto.Hash.Class (AbstractHash (..), HashFunction (..))

data CryptoniteFunc algo = CryptoniteFunc algo

instance Crypto.HashAlgorithm algo => HashFunction (CryptoniteFunc algo) where
    type HashResult (CryptoniteFunc algo) = Crypto.Digest algo
    unsafeAbstractHash = AbstractHash . Crypto.hash

instance Crypto.HashAlgorithm algo => Serialise (Crypto.Digest algo) where
    decode = do
        bs <- decode
        maybe (fail "invalid hash representation") return $
            Crypto.digestFromByteString (bs :: ByteString)

    encode = encode @ByteString . convert


-- | Class of types, which can be reconstructed from a 'ByteArray',
-- but only from a particular one.
-- TODO: naming is questionable, any ideas of better name?

module Disciplina.Crypto.ByteArrayStruct
       ( ByteArrayStruct (..)
       ) where

import Universum

import Crypto.Error (CryptoFailable, eitherCryptoError)
import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.ByteArray (ByteArray, ByteArrayAccess, convert)

-- TODO: maybe something better than 'String' for an error? Now
-- it's just because this 'String' goes to 'fail' from 'MonadFail'.
class ByteArrayAccess ba => ByteArrayStruct ba where
    reconstruct :: ByteArrayAccess ba' => ba' -> Either String ba

instance {-# OVERLAPPABLE #-}
    (ByteArrayAccess ba, ByteArray ba) => ByteArrayStruct ba where
    reconstruct = Right . convert

cfToEither :: CryptoFailable a -> Either String a
cfToEither = first show . eitherCryptoError

----------------------------------------------------------
-- Hashes
----------------------------------------------------------

instance Hash.HashAlgorithm algo => ByteArrayStruct (Hash.Digest algo) where
    reconstruct = maybeToRight "invalid hash representation" .
                  Hash.digestFromByteString

----------------------------------------------------------
-- Signatures
----------------------------------------------------------

instance ByteArrayStruct Ed25519.SecretKey where
    reconstruct = cfToEither . Ed25519.secretKey

instance ByteArrayStruct Ed25519.PublicKey where
    reconstruct = cfToEither . Ed25519.publicKey

instance ByteArrayStruct Ed25519.Signature where
    reconstruct = cfToEither . Ed25519.signature

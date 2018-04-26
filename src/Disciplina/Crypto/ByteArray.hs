
-- | Utilities for manipulating byte arrays.

module Disciplina.Crypto.ByteArray
       ( ByteArrayStruct (..)
       , hashBytesWithSalt
       , hashBytes
       ) where

import Universum

import Crypto.Error (CryptoFailable, eitherCryptoError)
import qualified Crypto.Hash as Crypto
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.ByteArray (ByteArray, ByteArrayAccess)
import qualified Data.ByteArray as BA
import qualified Data.Hashable as Hash
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | Class of types, which can be reconstructed from a 'ByteArray',
-- but only from a particular one.
-- TODO: naming is questionable, any ideas of better name?
-- TODO: maybe something better than 'String' for an error? Now
-- it's just because this 'String' goes to 'fail' from 'MonadFail'.
class ByteArrayAccess ba => ByteArrayStruct ba where
    reconstruct :: ByteArrayAccess ba' => ba' -> Either String ba

instance {-# OVERLAPPABLE #-}
    (ByteArrayAccess ba, ByteArray ba) => ByteArrayStruct ba where
    reconstruct = Right . BA.convert

cfToEither :: CryptoFailable a -> Either String a
cfToEither = first show . eitherCryptoError

----------------------------------------------------------
-- Hashes
----------------------------------------------------------

instance Crypto.HashAlgorithm algo =>
         ByteArrayStruct (Crypto.Digest algo) where
    reconstruct = maybeToRight "invalid hash representation" .
                  Crypto.digestFromByteString

----------------------------------------------------------
-- Signatures
----------------------------------------------------------

instance ByteArrayStruct Ed25519.SecretKey where
    reconstruct = cfToEither . Ed25519.secretKey

instance ByteArrayStruct Ed25519.PublicKey where
    reconstruct = cfToEither . Ed25519.publicKey

instance ByteArrayStruct Ed25519.Signature where
    reconstruct = cfToEither . Ed25519.signature

-----------------------------------------------------------
-- Utils
-----------------------------------------------------------

unsafeWithBytesAndLength ::
    ByteArrayAccess ba => ba -> (Int -> Ptr p -> IO a) -> a
unsafeWithBytesAndLength ba action =
    unsafeDupablePerformIO $ BA.withByteArray ba (action len)
  where
    !len = BA.length ba

-- | Hash any 'ByteArrayAccess' (in terms of 'Hashable' class)
-- efficiently. Useful for putting crypto hashes in hashmaps.
hashBytesWithSalt :: ByteArrayAccess ba => Int -> ba -> Int
hashBytesWithSalt s h = unsafeWithBytesAndLength h $ \len ptr ->
    Hash.hashPtrWithSalt ptr len s

hashBytes :: ByteArrayAccess ba => ba -> Int
hashBytes h = unsafeWithBytesAndLength h $ \len ptr ->
    Hash.hashPtr ptr len

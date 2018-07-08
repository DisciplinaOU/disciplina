-- | Utilities for manipulating byte arrays.

module Dscp.Crypto.ByteArray
       ( FromByteArray (..)
       , hashBytesWithSalt
       , hashBytes
       ) where

import Crypto.Error (CryptoFailable, eitherCryptoError)
import qualified Crypto.Hash as Crypto
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.ByteArray (ByteArray, ByteArrayAccess)
import qualified Data.ByteArray as BA
import qualified Data.Hashable as Hash
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | Class of types, which can be reconstructed from a 'ByteArray',
-- but only from a particular one.
class ByteArrayAccess ba => FromByteArray ba where
    fromByteArray :: ByteArrayAccess ba' => ba' -> Either String ba

instance {-# OVERLAPPABLE #-}
    (ByteArrayAccess ba, ByteArray ba) => FromByteArray ba where
    fromByteArray = pure . BA.convert

----------------------------------------------------------
-- Hashes
----------------------------------------------------------

instance Crypto.HashAlgorithm algo =>
         FromByteArray (Crypto.Digest algo) where
    fromByteArray = maybeToRight "Invalid hash size." .
                    Crypto.digestFromByteString

----------------------------------------------------------
-- Signatures
----------------------------------------------------------

cfToEither :: CryptoFailable a -> Either String a
cfToEither = first show . eitherCryptoError

-- | Fails if bytestring length is wrong.
instance FromByteArray Ed25519.SecretKey where
    fromByteArray = cfToEither . Ed25519.secretKey

-- | Fails if bytestring length is wrong.
instance FromByteArray Ed25519.PublicKey where
    fromByteArray = cfToEither . Ed25519.publicKey

-- | Fails if bytestring length is wrong.
instance FromByteArray Ed25519.Signature where
    fromByteArray = cfToEither . Ed25519.signature

-----------------------------------------------------------
-- Utils
-----------------------------------------------------------

-- | Perform some IO action which requires array length and memory
-- pointer to it unsafely.
--
-- Uses `unsafeDupablePerformIO`, which is very fast, but may occasionally be
-- run simultaneously in several threads, so IO action must be chosen carefully
-- (read-only operations are OK, beware of writes).
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

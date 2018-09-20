
-- | Utility functions for binary serialisation

module Dscp.Util.Serialise
       ( serialise'
       , deserialise'
       , deserialiseOrFail'
       , encodeCrcProtected
       , decodeCrcProtected
       ) where

import Codec.Serialise (DeserialiseFailure, Serialise (..), deserialise, deserialiseOrFail,
                        serialise)
import qualified Codec.Serialise.Decoding as D
import qualified Codec.Serialise.Encoding as E
import qualified Data.ByteString.Lazy as LBS
import Data.Digest.CRC32 (crc32)

---------------------------------------------------------------------------
-- Versions of 'serialise' functions for strict bytestrings
---------------------------------------------------------------------------

serialise' :: Serialise a => a -> ByteString
serialise' = LBS.toStrict . serialise

deserialise' :: Serialise a => ByteString -> a
deserialise' = deserialise . LBS.fromStrict

deserialiseOrFail' :: Serialise a => ByteString -> Either DeserialiseFailure a
deserialiseOrFail' = deserialiseOrFail . LBS.fromStrict

---------------------------------------------------------------------------
-- CRC protection
---------------------------------------------------------------------------

encodeCrcProtected :: Serialise a => a -> E.Encoding
encodeCrcProtected a =
    E.encodeListLen 2 <> E.encodeBytes body <> E.encodeWord32 (crc32 body)
  where
    body = serialise' a

decodeCrcProtected :: Serialise a => D.Decoder s a
decodeCrcProtected = do
    len <- D.decodeListLen
    unless (len == 2) $ fail "CRC protected: unexpected list length"
    bs <- D.decodeBytes
    expectedCrc <- D.decodeWord32
    let actualCrc = crc32 bs
    when (actualCrc /= expectedCrc) . fail $
        "decodeCrcProtected: invalid CRC, expected " ++ show expectedCrc ++
        ", actual " ++ show actualCrc
    either (fail . show) pure $ deserialiseOrFail' bs

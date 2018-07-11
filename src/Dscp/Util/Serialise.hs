
-- | Utility functions for binary serialisation

module Dscp.Util.Serialise
       ( serialise'
       , deserialise'
       , deserialiseOrFail'
       , encodeCrcProtected
       , decodeCrcProtected
       ) where

import Codec.CBOR.Write (toStrictByteString)
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

encodeCrcProtected :: (a -> E.Encoding) -> a -> E.Encoding
encodeCrcProtected coder a =
    body <> encode (crc32 $ toStrictByteString body)
  where
    body = coder a

-- TODO: don't do serialisation again only to compute CRC32 (somehow)
decodeCrcProtected :: Serialise a => D.Decoder s a -> D.Decoder s a
decodeCrcProtected decoder = do
    res <- decoder
    expectedCrc <- decode
    let bs = serialise res
        actualCrc = crc32 bs
    when (actualCrc /= expectedCrc) . fail $
        "decodeCrcProtected: invalid CRC, expected " ++ show expectedCrc ++
        ", actual " ++ show actualCrc
    return res


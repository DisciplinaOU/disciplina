
-- | Utility functions for binary serialisation

module Dscp.Util.Serialise
       ( encodeCrcProtected
       , decodeCrcProtected
       ) where

import Codec.CBOR.Write (toStrictByteString)
import Codec.Serialise (Serialise (..), serialise)
import qualified Codec.Serialise.Decoding as D
import qualified Codec.Serialise.Encoding as E
import Data.Digest.CRC32 (crc32)

encodeCrcProtected :: (a -> E.Encoding) -> a -> E.Encoding
encodeCrcProtected coder a =
    body <> encode (crc32 $ toStrictByteString body)
  where
    body = coder a

-- TODO: don't do serialisation again only to compute CRC32 (somehow)
decodeCrcProtected :: D.Decoder s a -> D.Decoder s a
decodeCrcProtected decoder = do
    res <- decoder
    expectedCrc <- decode
    let bs = serialise res
        actualCrc = crc32 bs
    when (actualCrc /= expectedCrc) . fail $
        "decodeCrcProtected: invalid CRC, expected " ++ show expectedCrc ++
        ", actual " ++ show actualCrc
    return res


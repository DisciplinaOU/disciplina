-- | Address and related.

module Dscp.Core.Foundation.Address
       ( Address (..)
       , mkAddr
       , addrToBase58
       , addrFromBase58
       , addrFromText
       , addrToText
       ) where

import Universum
import Codec.Serialise (Serialise (..))
import Data.ByteString.Base58 (Alphabet, bitcoinAlphabet, decodeBase58, encodeBase58)
import Fmt (Buildable (..), pretty)

import Dscp.Crypto.Impl (Hash, PublicKey, hash)
import Dscp.Crypto.Serialise ()
import Dscp.Util.Serialise (decodeCrcProtected, deserialiseOrFail', encodeCrcProtected, serialise')

-- | 'Address' datatype. Not 'newtype', because later it will
-- inevitably become more complex.
-- TODO: maybe we should use a shorter hash for address, like in Cardano?
data Address = Address
    { addrHash :: !(Hash PublicKey)
    } deriving (Eq, Ord, Show, Generic)

instance Hashable Address

-- | Address constructor from public key.
mkAddr :: PublicKey -> Address
mkAddr = Address . hash

instance Serialise Address where
    encode = encodeCrcProtected . addrHash
    decode = Address <$> decodeCrcProtected

-- | Address encoding alphabet. Should not contain characters
-- which are too similar to one another, to prevent accidental
-- mistyping.
addressAlphabet :: Alphabet
addressAlphabet = bitcoinAlphabet

addrToBase58 :: Address -> ByteString
addrToBase58 = encodeBase58 addressAlphabet . serialise'

addrFromBase58 :: ByteString -> Either Text Address
addrFromBase58 =
    maybeToRight base58Err . decodeBase58 addressAlphabet >=>
    first (\x -> "Failed to parse address: " <> show x) . deserialiseOrFail'
  where
    base58Err = "addrFromBase58: invalid base58 string"

addrFromText :: Text -> Either Text Address
addrFromText = addrFromBase58 . encodeUtf8

addrToText :: Address -> Text
addrToText = decodeUtf8 . addrToBase58

instance Buildable Address where
    build = build @Text . addrToText

instance ToText Address where
    toText = pretty

-- TODO: Provide this instance (this will require module rearrangement
-- due to 'Show' derivations in `Dscp.Core.Types` module)
-- instance Show Address where
--     show = toString . toText

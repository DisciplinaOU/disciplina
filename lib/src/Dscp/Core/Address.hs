
-- | Utils and formatting for addresses.

module Dscp.Core.Address
       ( addrToBase58
       , addrFromBase58
       , addrFromText
       ) where

import Data.ByteString.Base58 (Alphabet, bitcoinAlphabet, decodeBase58, encodeBase58)
import Fmt (build)
-- import qualified Text.Show

import Dscp.Core.Serialise ()
import Dscp.Core.Types (Address (..))
import Dscp.Util.Serialise (deserialiseOrFail', serialise')

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
    first show . deserialiseOrFail'
  where
    base58Err = "addrFromBase58: invalid base58 string"

addrFromText :: Text -> Either Text Address
addrFromText = addrFromBase58 . encodeUtf8

instance Buildable Address where
    build = build @Text . decodeUtf8 . addrToBase58

instance ToText Address where
    toText = pretty

-- TODO: Provide this instance (this will require module rearrangement
-- due to 'Show' derivations in `Dscp.Core.Types` module)
-- instance Show Address where
--     show = toString . toText

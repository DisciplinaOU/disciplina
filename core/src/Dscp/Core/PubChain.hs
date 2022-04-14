{-# LANGUAGE DerivingStrategies #-}

module Dscp.Core.PubChain
       ( -- * Solidity address
         PubAddress
       , pubAddrFromText

         -- * Solidity transaction ID
       , PubTxId
       , pubTxIdFromText
       ) where

import Universum

import Data.Aeson (ToJSON, FromJSON, encode, eitherDecode)
import qualified Data.Solidity.Prim.Address as Eth
import qualified Data.ByteArray.HexString as Hex
import Data.ByteArray (ByteArrayAccess, ByteArray)
import Fmt (Buildable (..))


-- | Address on the public chain (Ethereum/Binance Smart Chain).
-- Is equivalent to an Ethereum address starting with "0x".
newtype PubAddress = PubAddress
  { unPubAddress :: Eth.Address
  } deriving newtype (Show, Eq, Ord, Generic, IsString, ToJSON, FromJSON)

instance Hex.ToHex PubAddress where
  toHex = Eth.toHexString . unPubAddress

instance Hex.FromHex PubAddress where
  fromHex = fmap PubAddress . Eth.fromHexString

instance ToText PubAddress where
  toText = decodeUtf8 . Eth.toChecksum . encodeUtf8 . Hex.toText . Eth.toHexString . unPubAddress

instance Buildable PubAddress where
  build = build . toText

-- | Transform a `Text` string into `PubAddress`. Fails on ill-formed addresses
pubAddrFromText :: Text -> Either String PubAddress
pubAddrFromText = eitherDecode . encode

-- | ID of public transaction, encoded as hex string.
newtype PubTxId = PubTxId
  { unPubTxId :: Hex.HexString
  } deriving newtype (Show, Eq, Ord, IsString, ToJSON, Semigroup, Monoid, FromJSON, ByteArrayAccess, ByteArray)

instance Hex.ToHex PubTxId where
  toHex = unPubTxId

instance Hex.FromHex PubTxId where
  fromHex = Right . PubTxId

instance Buildable PubTxId where
  build = build . Hex.toText . Hex.toHex

instance ToText PubTxId where
  toText = Hex.toText . unPubTxId

-- | Transform a `Text` into `PubTxId`. Fails on ill-formed hex strings.
pubTxIdFromText :: Text -> Either String PubTxId
pubTxIdFromText = eitherDecode . encode

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}

-- | Message types we're using are bytestrings with a decimal ASCII
-- representation of the message tag. Obviously, more efficient
-- packing methods are available -- one can split 'Natural' into
-- several 'Word8' and convert it to BS directly, but it complicates
-- the debugging on early stages, hence is postponed.

module Dscp.Network.Messages
    (
      getMsgType
    , fromMsgType

    , PingBlk(..)
    , PongBlk(..)
    , PingTx(..)
    , PongTx(..)
    ) where

import Universum

import Codec.Serialise (Serialise)
import qualified Data.ByteString.Char8 as BS
import Loot.Network.Class (MsgType (..))
import Loot.Network.Message (Message (..), getMsgTag)

-- | Get a 'MsgType' related to the message specified (pack the
-- natural which is related to the message type).
getMsgType :: forall d. (Message d) => MsgType
getMsgType = MsgType $ BS.pack $ show $ getMsgTag @d

-- | Convert 'MsgType' to a natural number.
fromMsgType :: MsgType -> Maybe Natural
fromMsgType (MsgType bs) = readMaybe (BS.unpack bs)

-- | Type for messages from the workers to the witnessListeners.
data PingBlk = PingBlk deriving (Generic,Show)
instance Serialise PingBlk
instance Message PingBlk where type MsgTag PingBlk = 0

data PingTx = PingTx deriving (Generic,Show)
instance Serialise PingTx
instance Message PingTx where type MsgTag PingTx = 1

-- | Type for messages from the witnessListeners to the workers.
data PongBlk = PongBlk ByteString deriving (Generic,Show)
instance Serialise PongBlk
instance Message PongBlk where type MsgTag PongBlk = 2

data PongTx = PongTx ByteString deriving (Generic,Show)
instance Serialise PongTx
instance Message PongTx where type MsgTag PongTx = 3

{-# LANGUAGE DataKinds #-}

-- | Message types we're using are bytestrings with a decimal ASCII
-- representation of the message tag. Obviously, more efficient
-- packing methods are available -- one can split 'Natural' into
-- several 'Word8' and convert it to BS directly, but it complicates
-- the debugging on early stages, hence is postponed.

module Dscp.Network.Messages
    (
      PingBlk(..)
    , PongBlk(..)
    , PingTx(..)
    , PongTx(..)
    ) where

import Universum

import Codec.Serialise (Serialise)
import Loot.Network.Message (Message (..))

import Dscp.Network.Wrapped (MsgK)

----------------------------------------------------------------------------
-- Real messages (move them elsewhere)
----------------------------------------------------------------------------

-- | Type for messages from the workers to the witnessListeners.
data PingBlk = PingBlk deriving (Generic,Show)
instance Serialise PingBlk
instance Message MsgK PingBlk where type MsgTag MsgK PingBlk = 0

data PingTx = PingTx deriving (Generic,Show)
instance Serialise PingTx
instance Message MsgK PingTx where type MsgTag MsgK PingTx = 1

-- | Type for messages from the witnessListeners to the workers.
data PongBlk = PongBlk ByteString deriving (Generic,Show)
instance Serialise PongBlk
instance Message MsgK PongBlk where type MsgTag MsgK PongBlk = 2

data PongTx = PongTx ByteString deriving (Generic,Show)
instance Serialise PongTx
instance Message MsgK PongTx where type MsgTag MsgK PongTx = 3

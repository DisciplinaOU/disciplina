-- | Message types we're using are bytestrings with a decimal ASCII
-- representation of the message tag. Obviously, more efficient
-- packing methods are available -- one can split 'Natural' into
-- several 'Word8' and convert it to BS directly, but it complicates
-- the debugging on early stages, hence is postponed.

module Dscp.Witness.Messages
    (

      GetBlocksMsg (..)
    , BlocksMsg (..)
    , GetTipMsg (..)
    , TipMsg (..)

    , PubBlock (..)
    , SendTx (..)
    , PubTx (..)
    ) where

import Codec.Serialise (Serialise)
import Loot.Network.Message (Message (..))

import Dscp.Core
import Dscp.Network.Wrapped
import Dscp.Util

----------------------------------------------------------------------------
-- Messages
----------------------------------------------------------------------------

data GetBlocksMsg = GetBlocksMsg
    { gbOlder :: HeaderHash
    , gbNewer :: HeaderHash
    } deriving (Show, Generic)
instance Serialise GetBlocksMsg
instance Message MsgK GetBlocksMsg where type MsgTag MsgK GetBlocksMsg = 0

data BlocksMsg = NoBlocksMsg Text | BlocksMsg (OldestFirst NonEmpty Block) deriving (Show, Generic)
instance Serialise BlocksMsg
instance Message MsgK BlocksMsg where type MsgTag MsgK BlocksMsg = 1

data GetTipMsg = GetTipMsg deriving (Show, Generic)
instance Serialise GetTipMsg
instance Message MsgK GetTipMsg where type MsgTag MsgK GetTipMsg = 2

data TipMsg = TipMsg Block deriving (Show, Generic)
instance Serialise TipMsg
instance Message MsgK TipMsg where type MsgTag MsgK TipMsg = 3

----------------------------------------------------------------------------
-- Publications
----------------------------------------------------------------------------

data PubBlock = PubBlock Block deriving (Show,Generic)
instance Serialise PubBlock
instance Message SubK PubBlock where type MsgTag SubK PubBlock = 100

data SendTx = SendTx { getSendTx :: GTxWitnessed } deriving (Generic,Show)
instance Serialise SendTx
instance Message MsgK SendTx where type MsgTag MsgK SendTx = 4

data PubTx = PubTx { getPubTx :: GTxWitnessed } deriving (Generic,Show)
instance Serialise PubTx
instance Message SubK PubTx where type MsgTag SubK PubTx = 102

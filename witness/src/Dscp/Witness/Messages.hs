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
    , DistributeBlock (..)
    , RetranslateTx (..)
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
instance Message SubK PubBlock where type MsgTag SubK PubBlock = 4

data DistributeBlock = DistributeBlock Block deriving (Generic,Show)
instance Serialise DistributeBlock
instance Message MsgK DistributeBlock where type MsgTag MsgK DistributeBlock = 5

data RetranslateTx = RetranslateTx GTxWitnessed deriving (Generic,Show)
instance Serialise RetranslateTx
instance Message SubK RetranslateTx where type MsgTag SubK RetranslateTx = 6

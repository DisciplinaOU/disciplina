-- | Message types we're using are bytestrings with a decimal ASCII
-- representation of the message tag. Obviously, more efficient
-- packing methods are available -- one can split 'Natural' into
-- several 'Word8' and convert it to BS directly, but it complicates
-- the debugging on early stages, hence is postponed.

module Dscp.Witness.Messages
    (
      PubBlock (..)
    ) where

import Codec.Serialise (Serialise)
import Loot.Network.Message (Message (..))

import Dscp.Core
import Dscp.Network.Wrapped


data PubBlock = PubBlock Block deriving (Generic,Show)
instance Serialise PubBlock
instance Message SubK PubBlock where type MsgTag SubK PubBlock = 1

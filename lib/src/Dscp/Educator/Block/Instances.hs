module Dscp.Educator.Block.Instances () where

import Codec.Serialise (Serialise (..))

import Dscp.Core.Serialise ()
import Dscp.Crypto (Hash, hash)
import Dscp.Crypto.Serialise ()
import Dscp.Educator.Block.Type (PrivateBlock (..), PrivateBlockBody (..), PrivateBlockHeader (..))
import Dscp.Util (HasId (..))

-- TODO: make well-defined Serialise instances instead of generic ones

-- | Block
instance Serialise PrivateBlockHeader
instance Serialise PrivateBlockBody
instance Serialise PrivateBlock

instance HasId PrivateBlock where
    type Id PrivateBlock = Hash PrivateBlock

    getId = hash


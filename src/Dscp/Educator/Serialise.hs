module Dscp.Educator.Serialise () where

import Codec.Serialise (Serialise (..))

import Dscp.Core.Serialise ()
import Dscp.Crypto.Serialise ()
import Dscp.Educator.Block (PrivateBlock (..), PrivateBlockBody (..), PrivateBlockHeader (..))
import Dscp.Educator.Txs (PrivateTx (..), PrivateTxAux (..), PrivateTxWitness (..))

-- TODO: make well-defined Serialise instances instead of generic ones

-- | Transactions
instance Serialise PrivateTx
instance Serialise PrivateTxWitness
instance Serialise PrivateTxAux

-- | Block
instance Serialise PrivateBlockHeader
instance Serialise PrivateBlockBody
instance Serialise PrivateBlock

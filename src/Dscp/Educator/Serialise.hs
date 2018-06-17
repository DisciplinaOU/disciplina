module Dscp.Educator.Serialise () where

import Codec.Serialise (Serialise (..))

import Dscp.Core.Serialise ()
import Dscp.Crypto.Serialise ()
import Dscp.Educator.Block (PrivateBlock (..), PrivateBlockBody (..), PrivateBlockHeader (..))
import Dscp.Educator.Txs (EducatorTxMsg (..), PrivateTx (..), PrivateTxAux (..),
                                PrivateTxPayload (..), PrivateTxWitness (..), StudentTxMsg (..))

-- TODO: make well-defined Serialise instances instead of generic ones

-- | Transactions
instance Serialise StudentTxMsg
instance Serialise EducatorTxMsg
instance Serialise PrivateTxPayload
instance Serialise PrivateTx
instance Serialise PrivateTxWitness
instance Serialise PrivateTxAux

-- | Block
instance Serialise PrivateBlockHeader
instance Serialise PrivateBlockBody
instance Serialise PrivateBlock

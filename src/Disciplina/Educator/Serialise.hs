module Disciplina.Educator.Serialise () where

import Codec.Serialise (Serialise (..))

import Disciplina.Core.Serialise ()
import Disciplina.Crypto.Serialise ()
import Disciplina.Educator.Block (PrivateBlockBody (..), PrivateBlockHeader (..),
                                  SizedMerkleRoot (..))
import Disciplina.Educator.Txs (EducatorTxMsg (..), PrivateTx (..), PrivateTxAux (..),
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
instance Serialise (SizedMerkleRoot a)
instance Serialise PrivateBlockHeader
instance Serialise PrivateBlockBody

module Dscp.Core.Foundation.Transactions.Instances () where

import Codec.Serialise (Serialise (..))

import Dscp.Core.Foundation.Transactions.Types

instance Serialise Coin
instance Serialise TxInAcc
instance Serialise TxOut
instance Serialise Tx
instance Serialise TxWitness
instance Serialise TxWitnessed
instance Serialise Publication
instance Serialise LastPublication
instance Serialise PublicationsOf
instance Serialise PublicationNext
instance Serialise PublicationHead
instance Serialise PublicationTx
instance Serialise PublicationTxWitness
instance Serialise PublicationTxWitnessed
instance Serialise GTx
instance Serialise GTxWitnessed
instance Serialise BlockToSign
instance Serialise Difficulty
instance Serialise Header
instance Serialise Block
instance Serialise BlockBody

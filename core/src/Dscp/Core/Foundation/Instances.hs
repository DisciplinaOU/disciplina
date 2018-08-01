-- | Instances for foundaion types.

module Dscp.Core.Foundation.Instances where

import Codec.Serialise (Serialise (..))

import Dscp.Core.Foundation.Educator
import Dscp.Core.Foundation.Witness
import Dscp.Crypto (Hash, hash)
import Dscp.Crypto.Serialise ()
import Dscp.Util (HasId (..))

----------------------------------------------------------------------------
-- Educator
----------------------------------------------------------------------------

-- TODO: make well-defined Serialise instances instead of generic ones

-- | Block
instance Serialise PrivateBlockHeader
instance Serialise PrivateBlockBody
instance Serialise PrivateBlock

instance HasId PrivateBlock where
    type Id PrivateBlock = Hash PrivateBlockHeader

    getId = hash . _pbHeader

-- | Transactions
instance Serialise PrivateTx
instance Serialise PrivateTxWitness
instance Serialise PrivateTxAux

instance HasId PrivateTx where
    type Id PrivateTx = Hash PrivateTx
    getId = hash

-- TODO: move to well-specified serialisation instead of generic one.
deriving instance Serialise Course
deriving instance Serialise Subject


instance Serialise Grade
instance Serialise ATGNode
instance Serialise ATGEdge

deriving instance Serialise ATGDelta
deriving instance Serialise ATG

instance Serialise Assignment
instance Serialise AssignmentType
instance Serialise Submission
instance Serialise SubmissionWitness
instance Serialise SignedSubmission
instance Serialise DocumentType

----------------------------------------------------------------------------
-- Witness
----------------------------------------------------------------------------

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
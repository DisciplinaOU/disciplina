module Dscp.Core.Serialise () where

import Codec.Serialise (Serialise (..))

import Dscp.Core.Types
import Dscp.Util.Serialise (decodeCrcProtected, encodeCrcProtected)

-- TODO: move to well-specified serialisation instead of generic one.
deriving instance Serialise Course
deriving instance Serialise Subject

instance Serialise Address where
    encode = encodeCrcProtected . addrHash
    decode = Address <$> decodeCrcProtected

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

instance Serialise Coin
instance Serialise TxInAcc
instance Serialise TxOut
instance Serialise Tx
instance Serialise TxWitness
instance Serialise TxWitnessed
instance Serialise GTx
instance Serialise GTxWitnessed
instance Serialise BlockToSign
instance Serialise Difficulty
instance Serialise Header
instance Serialise Block
instance Serialise BlockBody

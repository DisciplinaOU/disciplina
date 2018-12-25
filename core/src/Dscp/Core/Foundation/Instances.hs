-- | Instances for foundaion types.

module Dscp.Core.Foundation.Instances where

import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLen, decodeWord)
import Codec.Serialise.Encoding (encodeListLen, encodeWord)

import Dscp.Core.Foundation.Coin
import Dscp.Core.Foundation.Educator
import Dscp.Core.Foundation.Witness
import Dscp.Crypto.Impl
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

instance Serialise ATGSubjectChange
deriving instance Serialise ATGDelta
deriving instance Serialise ATG

instance Serialise Assignment
instance Serialise AssignmentType
instance Serialise SubmissionWitness
instance Serialise SignedSubmission
instance Serialise DocumentType

instance Serialise Submission where
    encode (Submission s c a) = mconcat
        [ encodeListLen 4
        , encodeWord 0
        , encode s
        , encode c
        , encode a
        ]

    decode = do
        len <- decodeListLen
        tag <- decodeWord
        case (len, tag) of
            (4, 0) -> Submission <$> decode <*> decode <*> decode
            _      -> fail "Invalid Submission encoding"

----------------------------------------------------------------------------
-- Witness
----------------------------------------------------------------------------

-- | TODO: CBOR uncompromisingly seralises numbers in variable-length manner,
-- but we actually don't want fees to increase over time as corresponding
-- account is used.
instance Serialise Nonce

instance Serialise Coin
instance Serialise TxInAcc
instance Serialise TxOut
instance Serialise Tx
instance Serialise TxWitness
instance Serialise TxWitnessed
instance Serialise PublicationTx
instance Serialise PublicationTxWitness
instance Serialise PublicationTxWitnessed
instance Serialise GTx
instance Serialise GTxId
instance Serialise GTxWitnessed
instance Serialise BlockToSign
instance Serialise SlotId
instance Serialise Difficulty
instance Serialise Header
instance Serialise Block
instance Serialise BlockBody

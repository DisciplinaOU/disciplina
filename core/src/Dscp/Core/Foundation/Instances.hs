-- | Instances for foundaion types.

module Dscp.Core.Foundation.Instances where

import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLen, decodeWord)
import Codec.Serialise.Encoding (encodeListLen, encodeWord)
import Data.Coerce (coerce)
import Data.Time.Calendar (Day (..))

import Dscp.Core.Foundation.Coin
import Dscp.Core.Foundation.Educator
import Dscp.Core.Foundation.Witness
import Dscp.Crypto.Impl
import Dscp.Crypto.Serialise ()
import Dscp.Util

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
instance Serialise PrivateGrade
instance Serialise PrivateCertification
instance Serialise PrivateTxWitness
instance Serialise PrivateTxAux
instance Serialise GradingScale
instance Serialise CertificateGrade
instance Serialise SignedCertificateGrade
instance Serialise Language

instance HasId PrivateGrade where
    type Id PrivateGrade = Hash PrivateGrade
    getId = hash

instance HasId PrivateCertification where
    type Id PrivateCertification = Hash PrivateCertification
    getId = hash

instance HasId PrivateTx where
    type Id PrivateTx = Hash PrivateTx
    getId = \case
        PrivateTxGrade         grade -> coerce (getId grade)
        PrivateTxCertification cert  -> coerce (getId cert)

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
instance Serialise (DocumentType a)

instance Serialise Day where
    encode = encode . toModifiedJulianDay
    decode = ModifiedJulianDay <$> decode

instance Serialise EducationForm
instance Serialise CertificateFullInfo
instance Serialise CertificateMeta

instance HasId CertificateMeta where
    type Id CertificateMeta = Hash CertificateMeta
    getId = hash

instance HasId CertificateFullInfo where
    type Id CertificateFullInfo = Id CertificateMeta
    getId = getId . cfiMeta

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

instance Serialise ItemDesc where
    encode t = encode $ unItemDesc t
    decode = leftToFail . toItemDesc =<< decode

instance Serialise Timestamp where
    encode t = encode $ unTimestamp t
    decode = toTimestamp <$> decode

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

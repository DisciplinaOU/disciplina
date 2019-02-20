
module Dscp.Core.Foundation.Educator.Submission where

import Control.Lens (Getter, makeLenses, makePrisms, to)

import Dscp.Core.Foundation.Educator.Student
import Dscp.Core.Foundation.Educator.Assignment
import Dscp.Core.Foundation.Educator.DocumentType
import Dscp.Crypto
import Dscp.Util

-- | Student submissions
data Submission = Submission
    { _sStudentId      :: !(Id Student)
    -- ^ Student who created this submission
    , _sContentsHash   :: !(Hash Raw)
    -- ^ Hash of submission contents
    , _sAssignmentHash :: !(Hash Assignment)
    -- ^ Assignment of this submission
    } deriving (Eq, Ord, Show, Generic)

-- | A hash which indicates that a submission or an assignment
-- are offline.
-- TODO: make a more comprehensible and easily documentable value?...
offlineHash :: Hash Raw
offlineHash = unsafeHash ("offline" :: ByteString)

_sDocumentType :: Submission -> DocumentType
_sDocumentType = documentType . _sContentsHash

sDocumentType :: Getter Submission DocumentType
sDocumentType = to _sDocumentType

instance HasHash Submission => HasId Submission where
    type Id Submission = Hash Submission
    getId = hash

-- | Type alias for Submission signature.
type SubmissionSig = Signature (Id Submission)

-- | Witness contains data required to verify transaction.
data SubmissionWitness = SubmissionWitness
    { _swKey :: !PublicKey
    , _swSig :: !SubmissionSig
    } deriving (Show, Eq, Ord, Generic)

-- | Datatype for verifiable transaction (transaction with a witness)
data SignedSubmission = SignedSubmission
    { _ssSubmission :: !Submission
    -- ^ Student submission
    , _ssWitness    :: !SubmissionWitness
    -- ^ Submission witness
    } deriving (Eq, Ord, Show, Generic)

instance HasHash Submission => HasId SignedSubmission where
    type Id SignedSubmission = Hash Submission
    getId = hash . _ssSubmission

makeLenses ''Submission
makeLenses ''SubmissionWitness
makeLenses ''SignedSubmission

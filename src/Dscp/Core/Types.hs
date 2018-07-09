
-- | Core types used across Disciplina codebase.

module Dscp.Core.Types
       ( Address (..)
       , mkAddr
       , SubjectId (..)
       , Grade (..)
       , StudentId
       , EducatorId
       , CourseId (..)
       , Assignment (..)
       , AssignmentType (..)
       , AssignmentId
       , Submission (..)
       , IntegrationType (..)
       , SubmissionSig
       , SignedSubmission (..)
       , SubmissionWitness (..)
       , _aIntegrationType
       , aIntegrationType
       , aCourseId
       , aContentsHash
       , aType
       , aAssignment
       , _sIntegrationType
       , sIntegrationType
       , sStudentId
       , sContentsHash
       , sAssignment
       , swKey
       , swSig
       , ssSubmission
       , ssWitness

       -- * Activity Type Graph
       , ATGDelta (..)
       , ATGNode (..)
       , atgnSubjectId
       , atgnChildren
       , ATGEdge (..)
       , atgeWeight
       , atgeChild
       , ATG (..)
       ) where

import Control.Lens (Getter, makeLenses, to)
import Data.Map (Map)

import Dscp.Crypto (Hash, PublicKey, Raw, Signature, hash, unsafeHash)

-- | 'Address' datatype. Not 'newtype', because later it will
-- inevitably become more complex.
-- TODO: maybe we should use a shorter hash for address, like in Cardano?
data Address = Address
    { addrHash :: !(Hash PublicKey)
    } deriving (Eq, Ord, Show, Generic)

mkAddr :: PublicKey -> Address
mkAddr = Address . hash

-- | ID of particular subject.
newtype SubjectId = SubjectId
    { getSubjectId :: Word32
    } deriving (Eq, Ord, Show, Num)

-- | Assignment/course grade.
-- TODO: decide on final format of the grade.
data Grade = F | D | C | B | A
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | Student is identified by their public address.
type StudentId = Address

-- | Educator is identified by their public adddress.
type EducatorId = Address

-- | Educator's course ID is simply a 'Word32' too.
-- There's a mapping from course ID to a set of associated subject IDs.
newtype CourseId = CourseId
    { getCourseId :: Word32
    } deriving (Eq, Ord, Show, Num)

-- | Assignment can be either regular of final
data AssignmentType = Regular | CourseFinal
    deriving (Eq, Show, Generic)

-- | Assignment doesn't contain actual assignment contents - only hash of them.
data Assignment = Assignment
    { _aCourseId     :: !CourseId
    -- ^ Course this assignement belongs to
    , _aContentsHash :: !(Hash Raw)
    -- ^ Hash of assignment contents
    , _aType         :: !AssignmentType
    -- ^ Assignment type
    , _aAssignment   :: !Text
    -- ^ Description of assignment
    } deriving (Eq, Show, Generic)

-- | 'AssignmentId' is a hash of assignment contents,
-- which are stored off-chain.
type AssignmentId = Hash Assignment

-- | Student submissions
data Submission = Submission
    { _sStudentId    :: !StudentId
    -- ^ Student who created this submission
    , _sContentsHash :: !(Hash Raw)
    -- ^ Hash of submission contents
    , _sAssignment   :: !Assignment
    -- ^ Assignment of this submission
    } deriving (Eq, Show, Generic)

-- | A hash which indicates that a submission or an assignment
-- are offline.
-- TODO: make a more comprehensible and easily documentable value?...
offlineHash :: Hash Raw
offlineHash = unsafeHash ("offline" :: ByteString)

-- | Datatype to represent the notion of "offline"- and "online"-ness
-- of assignments and submissions.
-- TODO: come up with a better name maybe?
data IntegrationType = Online | Offline
    deriving (Eq, Show, Generic)

integrationType :: Hash Raw -> IntegrationType
integrationType h
    | h == offlineHash = Offline
    | otherwise        = Online

_aIntegrationType :: Assignment -> IntegrationType
_aIntegrationType = integrationType . _aContentsHash

aIntegrationType :: Getter Assignment IntegrationType
aIntegrationType = to _aIntegrationType

_sIntegrationType :: Submission -> IntegrationType
_sIntegrationType = integrationType . _sContentsHash

sIntegrationType :: Getter Submission IntegrationType
sIntegrationType = to _sIntegrationType

-- | Type alias for Submission hash
type SubmissionId = Hash Submission

-- | Type alias for Submission signature.
type SubmissionSig = Signature SubmissionId

-- | Witness contains data required to verify transaction.
data SubmissionWitness = SubmissionWitness
    { _swKey :: !PublicKey
    , _swSig :: !SubmissionSig
    } deriving (Show, Eq, Generic)

-- | Datatype for verifiable transaction (transaction with a witness)
data SignedSubmission = SignedSubmission
    { _ssSubmission :: !Submission
    -- ^ Student submission
    , _ssWitness    :: !SubmissionWitness
    -- ^ Submission witness
    } deriving (Eq, Show, Generic)

makeLenses ''Assignment
makeLenses ''Submission
makeLenses ''SubmissionWitness
makeLenses ''SignedSubmission

-- | ATGDelta is a diff for set of subjects which are taught by Educator.
-- Implemented as 'Map SubjectId Bool' to avoid representing invalid diffs
-- (like, subject is present simultaneously in added and removed sets).
-- TODO: maybe we should separately make up a library for such stuff,
-- like 'MapModifier'?
newtype ATGDelta = ATGDelta
    { getATGDelta :: Map SubjectId Bool
    } deriving (Show, Eq, Ord, Generic)

---------------------------------------------------------------------
-- Activity Type Graph
---------------------------------------------------------------------

{-

@flyingleafe: I implemented a version of ATG without "etc" vertices
because I don't quite see the reason for having them.

-}

-- | Activity Type Graph node. Implementation without "etc" vertices.
-- TODO: should we use 'Vector' for more efficient indexing? or we don't
-- care, because it should be in DB somehow anyway?
data ATGNode = ATGNode
    { _atgnSubjectId :: !SubjectId
    , _atgnChildren  :: ![ATGEdge]
    } deriving (Show, Eq, Generic)

-- | Edge pointing to the children node in ATG.
data ATGEdge = ATGEdge
    { _atgeWeight :: !Float
    , _atgeChild  :: !ATGNode
    } deriving (Show, Eq, Generic)

-- | Activity Type Graph itself is just a list of root nodes.
newtype ATG = ATG
    { getATGRoots :: [ATGNode]
    } deriving (Show, Eq, Generic)

makeLenses ''ATGNode
makeLenses ''ATGEdge
makeLenses ''ATG


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
       , SubmissionType
       , SignedSubmission (..)
       , SubmissionWitness (..)
       , SubmissionWitnessAux (..)

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

import Universum

import Control.Lens (makeLenses)
import Data.Map (Map)

import Dscp.Crypto (Hash, PublicKey, Raw, Signature, hash)

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
    { aCourseId   :: !CourseId
    -- ^ Course this assignement belongs to
    , aType       :: !AssignmentType
    -- ^ Assignment type
    , aAssignment :: !Text
    -- ^ Hash of assignment content
    } deriving (Eq, Show, Generic)

-- | 'AssignmentId' is a hash of assignment contents,
-- which are stored off-chain.
type AssignmentId = Hash Assignment

-- | Submission can be digital or offline
-- TODO, better description of how these differs
data SubmissionType = Digital | Offline
    deriving (Eq, Show, Generic)

-- | Student submissions
data Submission = Submission
    { sStudentId  :: !StudentId
    -- ^ Student who created this submission
    , sType       :: !SubmissionType
    -- ^ Submission type
    , sAssignment :: !Assignment
    -- ^ Assignment of this submission
    } deriving (Eq, Show, Generic)

-- | Type alias for Submission signature.
type SubmissionSig = Signature Submission

-- | Submission signature.
data SignedSubmission = SignedSubmission
    { ssSubmission :: !Submission
    -- ^ Student submission
    , ssWitness    :: !SubmissionWitness
    -- ^ Submission witness
    } deriving (Eq, Show, Generic)

-- | Witness contains data required to verify transaction.
-- Included 'PublicKey' belongs either to Student or Educator.
-- TODO: maybe we can say that Educator's key is already known
-- to everybody, and not include it into Educator's witness?
data SubmissionWitness = SubmissionWitness
    { _swKey :: !PublicKey
    , _swSig :: !SubmissionSig
    } deriving (Show, Eq, Generic)

-- | Datatype for verifiable transaction (transaction with a witness)
data SubmissionWitnessAux = SubmissionWitnessAux
    { _swaTx      :: !Submission
    , _swaWitness :: !SubmissionWitness
    } deriving (Show, Eq, Generic)


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

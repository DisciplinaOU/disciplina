
-- | Definitions of private transactions

module Disciplina.Educator.Txs
       ( PrivateTx (..)
       , PrivateTxId
       , PrivateTxPayload (..)
       , StudentTxMsg (..)
       , EducatorTxMsg (..)
       , PrivateTxWitness (..)
       , PrivateTxAux (..)
       ) where

import Universum

import Disciplina.Core.Types (AssignmentId, CourseId, EducatorId, Grade, StudentId)
import Disciplina.Crypto (Hash, PublicKey, Signature)

-- | Private transaction.
data PrivateTx = PrivateTx
    { _ptxStudentId  :: !StudentId
    -- ^ Every transaction relates to one particular student.
    , _ptxCourseId   :: !CourseId
    -- ^ Every transaction relates to one particular course.
    , _ptxEducatorId :: !EducatorId
    -- ^ Included to simplify educator signature checking
    -- on student's side.
    , _ptxPayload    :: !PrivateTxPayload
    -- ^ Actual contents of transaction.
    } deriving (Show, Eq, Generic)

type PrivateTxId = Hash PrivateTx

-- | Private transaction payload. Divided by two types: student
-- messages and educator messages, which have different signature
-- verification procedures.
data PrivateTxPayload
    = StudentTx  { _ptxStudentMsg  :: !StudentTxMsg }
    | EducatorTx { _ptxEducatorMsg :: !EducatorTxMsg }
    deriving (Show, Eq, Generic)

-- | Stub type for submissions. Submission transaction
-- doesn't contain actual submission contents - only hash of them.
data Submission

-- | Messages which can be sent by student.
data StudentTxMsg
    = Enroll
    -- ^ TODO: add some conditions for successful course enrollment maybe?
    | Submit
      { _stmAssignmentId :: !AssignmentId
      , _stmSubmission   :: !(Hash Submission)
      }
    deriving (Show, Eq, Generic)

-- | Messages which can be sent by educator.
data EducatorTxMsg
    = Assign
      -- @flyingleafe: I think that it's more straightforward if
      -- not a student takes an assignment (like currently described in YP),
      -- but an educator gives it to a student. That's how things work in
      -- most of the schools/universities anyway, and it also seems to be
      -- simpler to implement in terms of student-educator communication.
      { _etmAssignmentId :: !AssignmentId
      }
    | GradeAssignment
      { _etmAssignmentId :: !AssignmentId
      , _etmGrade        :: !Grade
      }
    | GradeCourse
      { _etmGrade :: !Grade
      }
    deriving (Show, Eq, Generic)

-- | Which data to sign in transaction.
-- 'PrivateTxId' is basically a hash of all transaction contents,
-- so it's sufficient to sign only that.
type PrivateTxSigData = PrivateTxId

-- | Type alias for private tx signature.
type PrivateTxSig = Signature PrivateTxSigData

-- | Witness contains data required to verify transaction.
-- Included 'PublicKey' belongs either to Student or Educator.
-- TODO: maybe we can say that Educator's key is already known
-- to everybody, and not include it into Educator's witness?
data PrivateTxWitness = PkWitness
    { _ptwKey :: !PublicKey
    , _ptwSig :: !PrivateTxSig
    } deriving (Show, Eq, Generic)

-- | Datatype for verifiable transaction (transaction with a witness)
data PrivateTxAux = PrivateTxAux
    { _ptaTx      :: !PrivateTx
    , _ptaWitness :: !PrivateTxWitness
    } deriving (Show, Eq, Generic)

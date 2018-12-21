module Dscp.Educator.DB.Error
       (  -- * Domain-level database errors (missing entites, mostly)
         DomainError (..)
       , DomainErrorItem (..)
       , DatabaseSemanticError (..)

         -- * Prisms for constructors
       , _AbsentError
       , _AlreadyPresentError
       , _SemanticError
       , _CourseDomain
       , _StudentDomain
       , _AssignmentDomain
       , _StudentCourseEnrollmentDomain
       , _StudentAssignmentSubscriptionDomain
       , _SubmissionDomain
       , _TransactionDomain
       , _BlockWithIndexDomain
       , _DeletingGradedSubmission
       , _StudentIsActiveError
       ) where

import Control.Lens (makePrisms)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Text.Buildable as B
import Fmt ((+|), (|+))

import Dscp.Core
import Dscp.Educator.DB.BlockData
import Dscp.Util

data DomainError
    = AbsentError DomainErrorItem
    | AlreadyPresentError DomainErrorItem
    | SemanticError DatabaseSemanticError
    deriving (Show, Eq)

data DomainErrorItem
    = CourseDomain
        { deCourseId :: Id Course }

    | StudentDomain
        { deStudentId :: Id Student }

    | AssignmentDomain
        { deAssignmentId :: Id Assignment }

    | StudentCourseEnrollmentDomain
        { deStudentId :: Id Student
        , deCourseId  :: Id Course }

    | StudentAssignmentSubscriptionDomain
        { deStudentId    :: Id Student
        , deAssignmentId :: Id Assignment }

    | SubmissionDomain
        { deSubmissionId :: Id Submission }

    | TransactionDomain
        { deTransactionId :: Id PrivateTx }

    | BlockWithIndexDomain
        { deBlockIdx :: BlockIdx }

    deriving (Show, Typeable, Eq)

-- | Logical errors.
data DatabaseSemanticError
    = StudentIsActiveError     (Id Student)
      -- ^ Student can't be deleted because it has activities.
    | DeletingGradedSubmission (Id Submission)
      -- ^ Submission has potentially published grade and thus can't be deleted.
    deriving (Show, Eq)

makePrisms ''DomainError
makePrisms ''DomainErrorItem
makePrisms ''DatabaseSemanticError

instance Exception DomainError

instance Buildable DomainErrorItem where
    build (CourseDomain id_) =
        "Course { id="+|id_|+" }"
    build (StudentDomain id_) =
        "Student { id="+|id_|+" }"
    build (AssignmentDomain id_) =
        "Assignment { id="+|id_|+" }"
    build (StudentCourseEnrollmentDomain student course) =
        "Enrollment { student="+|student|+", course="+|course|+" }"
    build (StudentAssignmentSubscriptionDomain student assignment) =
        "Student assignment { student="+|student|+", assignment="+|assignment|+" }"
    build (SubmissionDomain id_) =
        "Submission { id="+|id_|+" }"
    build (TransactionDomain id_) =
        "Transaction { id="+|id_|+" }"
    build (BlockWithIndexDomain idx) =
        "Block { idx="+|idx|+" }"

instance Buildable DatabaseSemanticError where
    build (StudentIsActiveError id_) =
        "Student "+|id_|+" is active"
    build (DeletingGradedSubmission id_) =
        "Submission "+|id_|+" cannot be deleted, it's already graded"

instance Buildable DomainError where
    build (AbsentError entity) =
        "Entity ["+|entity|+"] is absent"
    build (AlreadyPresentError entity) =
        "Entity ["+|entity|+"] already exists"
    build (SemanticError err) =
        "Semantic error: " <> B.build err

deriveJSON defaultOptions ''DomainErrorItem
deriveJSON defaultOptions ''DatabaseSemanticError
deriveJSON defaultOptions ''DomainError

-- | Mid-layer between servant and sqlite endpoints.

module Dscp.Educator.Web.Student.Logic
    ( requestToSignedSubmission
    , studentMakeSubmissionVerified
    , studentGetAssignment
    , studentGetAllAssignments
    , studentGetSubmission
    , studentGetAllSubmissions
    ) where

import Data.Default (def)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQLite
import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Student.Error (APIError (..))
import Dscp.Educator.Web.Student.Types
import Dscp.Educator.Web.Student.Util (verifyStudentSubmission)
import Dscp.Educator.Web.Types
import Dscp.Util

requestToSignedSubmission
    :: MonadEducatorWeb ctx m
    => NewSubmission -> m SignedSubmission
requestToSignedSubmission ns = do
    assignment <- invoke $ getAssignment (nsAssignmentHash ns)
        `assertJust` AbsentError (AssignmentDomain $ nsAssignmentHash ns)
    let submission = Submission
            { _sStudentId = nsOwner ns
            , _sAssignmentHash = hash assignment
            , _sContentsHash = nsContentsHash ns
            }
    return SignedSubmission
        { _ssSubmission = submission
        , _ssWitness = nsWitness ns
        }

-- | Verifies given user can create make submission and makes it.
studentMakeSubmissionVerified
    :: MonadEducatorWeb ctx m
    => Student -> NewSubmission -> m SubmissionStudentInfo
studentMakeSubmissionVerified student newSubmission = do
    signedSubmission <- requestToSignedSubmission newSubmission
    verifyStudentSubmission student signedSubmission
        & leftToThrow BadSubmissionSignature
    transactW $ do
        submissionId <- submitAssignment signedSubmission
        studentGetSubmission student submissionId

-- | Get exactly one assignment.
studentGetAssignment
    :: MonadEducatorWebQuery m
    => Student
    -> Hash Assignment
    -> DBT WithinTx w m AssignmentStudentInfo
studentGetAssignment student assignH =
    commonGetAssignments StudentCase student def
        { afAssignmentHash = Just assignH }
        >>= listToMaybeWarn "assignment"
        >>= nothingToThrow (AbsentError $ AssignmentDomain assignH)

-- | Get all student assignments.
studentGetAllAssignments
    :: MonadEducatorWebQuery m
    => Student
    -> DBT WithinTx w m [AssignmentStudentInfo]
studentGetAllAssignments student =
    commonGetAssignments StudentCase student def

studentGetSubmission
    :: MonadEducatorWebQuery m
    => Student
    -> Hash Submission
    -> DBT t w m SubmissionStudentInfo
studentGetSubmission student submissionH = do
    commonGetSubmissions StudentCase def
        { sfStudent = Just student, sfSubmissionHash = Just submissionH }
        >>= listToMaybeWarn "submission"
        >>= nothingToThrow (AbsentError $ SubmissionDomain submissionH)

-- | Get all student submissions.
studentGetAllSubmissions
    :: MonadEducatorWebQuery m
    => Student
    -> DBT t w m [SubmissionStudentInfo]
studentGetAllSubmissions student =
    commonGetSubmissions StudentCase def{ sfStudent = Just student }

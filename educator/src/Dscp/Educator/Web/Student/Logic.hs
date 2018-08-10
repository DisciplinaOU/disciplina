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
import Dscp.Educator.Web.Student.Queries
import Dscp.Educator.Web.Student.Types
import Dscp.Educator.Web.Student.Util (verifyStudentSubmission)
import Dscp.Educator.Web.Types
import Dscp.Util

requestToSignedSubmission
    :: MonadStudentAPIQuery m
    => NewSubmission -> m SignedSubmission
requestToSignedSubmission ns = do
    assignment <- getAssignment (nsAssignmentHash ns)
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
    :: MonadStudentAPIQuery m
    => Student -> NewSubmission -> m SubmissionStudentInfo
studentMakeSubmissionVerified student newSubmission = do
    signedSubmission <- requestToSignedSubmission newSubmission
    verifyStudentSubmission student signedSubmission
        & leftToThrow BadSubmissionSignature
    sqlTransaction $ do
        submissionId <- submitAssignment signedSubmission
        studentGetSubmission student submissionId

-- | Get exactly one assignment.
studentGetAssignment
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student
    -> Hash Assignment
    -> m AssignmentStudentInfo
studentGetAssignment student assignH = do
    commonGetAssignments StudentCase student def
        { afAssignmentHash = Just assignH }
        >>= listToMaybeWarn "assignment"
        >>= nothingToThrow (AbsentError $ AssignmentDomain assignH)

-- | Get all student assignments.
studentGetAllAssignments
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student
    -> m [AssignmentStudentInfo]
studentGetAllAssignments student = commonGetAssignments StudentCase student def

-- | Get exactly one submission.
studentGetSubmission
    :: MonadStudentAPIQuery m
    => Student
    -> Hash Submission
    -> m SubmissionStudentInfo
studentGetSubmission student submissionH = do
    commonGetSubmissions StudentCase def
        { sfStudent = Just student, sfSubmissionHash = Just submissionH }
        >>= listToMaybeWarn "submission"
        >>= nothingToThrow (AbsentError $ SubmissionDomain submissionH)

-- | Get all student submissions.
studentGetAllSubmissions
    :: MonadStudentAPIQuery m
    => Student
    -> m [SubmissionStudentInfo]
studentGetAllSubmissions student =
    commonGetSubmissions StudentCase def{ sfStudent = Just student }

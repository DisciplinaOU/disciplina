-- | Mid-layer between servant and sqlite endpoints.

module Dscp.Educator.Web.Student.Logic
    ( requestToSignedSubmission
    , studentMakeSubmissionVerified
    ) where

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQLite (sqlTransaction)
import Dscp.DB.SQLite.Queries
import Dscp.Educator.Web.Student.Error (APIError (..))
import Dscp.Educator.Web.Student.Queries
import Dscp.Educator.Web.Student.Types
import Dscp.Educator.Web.Student.Util (verifyStudentSubmission)
import Dscp.Util (assertJust, leftToThrow)

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

-- | Mid-layer between servant and sqlite endpoints.

module Dscp.Educator.Web.Student.Logic
    ( requestToSignedSubmission
    , studentMakeSubmissionVerified
    ) where

import Dscp.Core
import Dscp.DB.SQLite (sqlTransaction)
import Dscp.DB.SQLite.Queries
import Dscp.Educator.Web.Student.Error (APIError (..))
import Dscp.Educator.Web.Student.Queries
import Dscp.Educator.Web.Student.Util (verifyStudentSubmission)
import Dscp.Educator.Web.Types
import Dscp.Util (assertJust, leftToThrow)

requestToSignedSubmission
    :: MonadStudentAPIQuery m
    => NewSubmission -> m SignedSubmission
requestToSignedSubmission ns = do
    assignment <- getAssignment (nsAssignmentHash ns)
        `assertJust` AssignmentDoesNotExist (nsAssignmentHash ns)
    let submission = Submission
            { _sStudentId = nsOwner ns
            , _sAssignment = assignment
            , _sContentsHash = nsContentsHash ns
            }
    return SignedSubmission
        { _ssSubmission = submission
        , _ssWitness = nsWitness ns
        }

-- | Verifies given user can create make submission and makes it.
studentMakeSubmissionVerified
    :: MonadStudentAPIQuery m
    => Student -> NewSubmission -> m SubmissionInfo
studentMakeSubmissionVerified student newSubmission = do
    signedSubmission <- requestToSignedSubmission newSubmission
    verifyStudentSubmission student signedSubmission
        & leftToThrow BadSubmissionSignature
    sqlTransaction $ do
        submissionId <- studentMakeSubmission signedSubmission
        studentGetSubmission student submissionId

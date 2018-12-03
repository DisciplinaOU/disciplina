-- | Mid-layer between servant and sqlite endpoints.

module Dscp.Educator.Web.Student.Logic
    ( requestToSignedSubmission
    , studentMakeSubmissionVerified
    ) where

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQLite
import Dscp.Educator.DB
import Dscp.Educator.Web.Student.Error (APIError (..))
import Dscp.Educator.Web.Student.Queries
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

-- | Mid-layer between servant and sqlite endpoints.

module Dscp.Educator.Web.Student.Logic
    ( requestToSignedSubmission
    , makeSubmissionVerified
    ) where

import qualified Dscp.Core as Core
import Dscp.DB.SQLite (sqlTransaction)
import qualified Dscp.DB.SQLite.Queries as CoreQueries
import Dscp.Educator.Web.Student.Error (APIError (..))
import qualified Dscp.Educator.Web.Student.Queries as Queries
import Dscp.Educator.Web.Student.Util (verifyStudentSubmission)
import Dscp.Educator.Web.Types (NewSubmission (..), Student, Submission (..), nsOwner)
import Dscp.Util (assertJust, leftToThrow)

requestToSignedSubmission
    :: Queries.MonadStudentAPIQuery m
    => NewSubmission -> m Core.SignedSubmission
requestToSignedSubmission ns = do
    assignment <- CoreQueries.getAssignment (nsAssignmentHash ns)
        `assertJust` CoreQueries.AssignmentDoesNotExist (nsAssignmentHash ns)
    let submission = Core.Submission
            { _sStudentId = nsOwner ns
            , _sAssignment = assignment
            , _sContentsHash = nsContentsHash ns
            }
    return Core.SignedSubmission
        { _ssSubmission = submission
        , _ssWitness = nsWitness ns
        }

-- | Verifies given user can create make submission and makes it.
makeSubmissionVerified
    :: Queries.MonadStudentAPIQuery m
    => Student -> NewSubmission -> m Submission
makeSubmissionVerified student newSubmission = do
    signedSubmission <- requestToSignedSubmission newSubmission
    verifyStudentSubmission student signedSubmission
        & leftToThrow BadSubmissionSignature
    sqlTransaction $ do
        submissionId <- Queries.makeSubmission signedSubmission
        Queries.getSubmission student submissionId

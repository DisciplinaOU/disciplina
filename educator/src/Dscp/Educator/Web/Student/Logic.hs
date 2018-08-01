-- | Mid-layer between servant and sqlite endpoints.

module Dscp.Educator.Web.Student.Logic
    ( makeSubmissionVerified
    ) where

import qualified Dscp.Core as Core
import Dscp.DB.SQLite (sqlTransaction)
import Dscp.Educator.Web.Student.Error (APIError (..))
import qualified Dscp.Educator.Web.Student.Queries as Queries
import Dscp.Educator.Web.Student.Types (Student, Submission)
import Dscp.Educator.Web.Student.Util (verifyStudentSubmission)
import Dscp.Util (leftToThrow)

-- | Verifies given user can create make submission and makes it.
makeSubmissionVerified
    :: Queries.MonadStudentAPIQuery m
    => Student -> Core.SignedSubmission -> m Submission
makeSubmissionVerified student signedSubmission = do
    verifyStudentSubmission student signedSubmission
        & leftToThrow BadSubmissionSignature
    sqlTransaction $ do
        submissionId <- Queries.makeSubmission signedSubmission
        Queries.getSubmission student submissionId

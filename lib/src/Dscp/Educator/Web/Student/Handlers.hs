
-- | Student API handlers

module Dscp.Educator.Web.Student.Handlers
       ( servantHandlers
       ) where

import Data.Time.Clock (UTCTime)
import Servant

import qualified Dscp.Core.Types as Core
import Dscp.Crypto (Hash, deterministic, genSecretKey, toPublic)
import Dscp.DB.SQLite (sqlTransaction)
import qualified Dscp.DB.SQLite.Queries as CoreQueries
import Dscp.Educator.Launcher (EducatorWorkMode)
import Dscp.Educator.Web.Student.API (StudentAPI)
import qualified Dscp.Educator.Web.Student.Queries as Queries
import Dscp.Util (leftToThrow)

import Dscp.Educator.Web.Student.Types (Assignment, BlkProof, Course, IsEnrolled (..), IsFinal (..),
                                        Student, Submission)
import Dscp.Educator.Web.Student.Util (verifySignedSubmission)

servantHandlers :: EducatorWorkMode m => ServerT StudentAPI m
servantHandlers
    =    getCourses
    :<|> getCourse
    :<|> getAssignments
    :<|> getAssignment
    :<|> getSubmissions
    :<|> getSubmission
    :<|> makeSubmission
    :<|> deleteSubmission
    :<|> getProofs

-- TODO [DSCP-141]: remove
student :: Student
student = Core.mkAddr . toPublic $ deterministic "" genSecretKey

{- GRAND TODO LIST

1. Student authentication ([DSCP-141]).

2. Errors:
2.1. Catch and rethrow SQL DomainErrors?
2.2. Where are exceptions turned into 'ServantErr'?


-}

getCourses
    :: EducatorWorkMode m
    => Maybe IsEnrolled -> m [Course]
getCourses isEnrolled =
    sqlTransaction $ Queries.getCourses student isEnrolled

getCourse
    :: EducatorWorkMode m
    => Core.Course -> m Course
getCourse courseId =
    Queries.getCourse student courseId

getAssignments
    :: EducatorWorkMode m
    => Maybe Core.Course -> Maybe Core.DocumentType -> Maybe IsFinal
    -> m [Assignment]
getAssignments mcourseId mdocType mIsFinal =
    sqlTransaction $ Queries.getAssignments student mcourseId mdocType mIsFinal

getAssignment
    :: EducatorWorkMode m
    => Hash Core.Assignment -> m Assignment
getAssignment assignH =
    sqlTransaction $ Queries.getAssignment student assignH

getSubmissions
    :: EducatorWorkMode m
    => Maybe Core.Course -> Maybe (Hash Core.Assignment) -> Maybe Core.DocumentType
    -> m [Submission]
getSubmissions mcourseId massignH mdocType =
    sqlTransaction $ Queries.getSubmissions student mcourseId massignH mdocType

getSubmission
    :: EducatorWorkMode m
    => Hash Core.Submission
    -> m Submission
getSubmission submissionH =
    sqlTransaction $ Queries.getSubmission student submissionH

makeSubmission
    :: EducatorWorkMode m
    => Core.SignedSubmission -> m Submission
makeSubmission signedSubmission = do
    verifySignedSubmission student signedSubmission
        & leftToThrow id
    submissionId <- CoreQueries.submitAssignment signedSubmission
    getSubmission submissionId

deleteSubmission
    :: EducatorWorkMode m
    => Hash Core.Submission
    -> m ()
deleteSubmission submissionH =
    sqlTransaction $ Queries.deleteSubmission student submissionH

getProofs
    :: EducatorWorkMode m
    => Maybe UTCTime -> m [BlkProof]
getProofs sinceF =
    sqlTransaction $ Queries.getProofs student sinceF

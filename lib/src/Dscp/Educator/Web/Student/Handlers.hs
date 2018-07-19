-- | Student API handlers

module Dscp.Educator.Web.Student.Handlers
       ( servantHandlers
       ) where

import Data.Time.Clock (UTCTime)
import Servant

import qualified Dscp.Core.Types as Core
import Dscp.Crypto (Hash, genSecretKey, toPublic, withIntSeed)
import Dscp.DB.SQLite (sqlTransaction)
import Dscp.Educator.Launcher (EducatorWorkMode)
import Dscp.Educator.Web.Student.API (StudentAPI)
import Dscp.Educator.Web.Student.Logic (makeSubmissionVerified)
import qualified Dscp.Educator.Web.Student.Queries as Queries

import Dscp.Educator.Web.Student.Types (Assignment, BlkProof, Course, IsEnrolled (..), IsFinal (..),
                                        Student, Submission)

servantHandlers :: EducatorWorkMode ctx m => ServerT StudentAPI m
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
student = Core.mkAddr . toPublic $ withIntSeed 123 genSecretKey

getCourses
    :: EducatorWorkMode ctx m
    => Maybe IsEnrolled -> m [Course]
getCourses isEnrolled =
    sqlTransaction $ Queries.getCourses student isEnrolled

getCourse
    :: EducatorWorkMode ctx m
    => Core.Course -> m Course
getCourse courseId =
    Queries.getCourse student courseId

getAssignments
    :: EducatorWorkMode ctx m
    => Maybe Core.Course -> Maybe Core.DocumentType -> Maybe IsFinal
    -> m [Assignment]
getAssignments mcourseId mdocType mIsFinal =
    sqlTransaction $ Queries.getAssignments student mcourseId mdocType mIsFinal

getAssignment
    :: EducatorWorkMode ctx m
    => Hash Core.Assignment -> m Assignment
getAssignment assignH =
    sqlTransaction $ Queries.getAssignment student assignH

getSubmissions
    :: EducatorWorkMode ctx m
    => Maybe Core.Course -> Maybe (Hash Core.Assignment) -> Maybe Core.DocumentType
    -> m [Submission]
getSubmissions mcourseId massignH mdocType =
    sqlTransaction $ Queries.getSubmissions student mcourseId massignH mdocType

getSubmission
    :: EducatorWorkMode ctx m
    => Hash Core.Submission
    -> m Submission
getSubmission submissionH =
    sqlTransaction $ Queries.getSubmission student submissionH

makeSubmission
    :: EducatorWorkMode ctx m
    => Core.SignedSubmission -> m Submission
makeSubmission signedSubmission =
    makeSubmissionVerified student signedSubmission

deleteSubmission
    :: EducatorWorkMode ctx m
    => Hash Core.Submission
    -> m ()
deleteSubmission submissionH =
    sqlTransaction $ Queries.deleteSubmission student submissionH

getProofs
    :: EducatorWorkMode ctx m
    => Maybe UTCTime -> m [BlkProof]
getProofs sinceF =
    sqlTransaction $ Queries.getProofs student sinceF

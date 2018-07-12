
-- | Student API handlers

module Dscp.Educator.Web.Student.Handlers
       ( servantHandlers
       ) where

import Data.Time.Clock (UTCTime)
import Servant
import qualified UnliftIO as UIO

import qualified Dscp.Core.Types as Core
import Dscp.Crypto (Hash)

import Dscp.Educator.Launcher (EducatorWorkMode)
import Dscp.Educator.Web.Student.API (StudentAPI)
import Dscp.Educator.Web.Student.Types (Assignment, BlkProof, Course, Submission)

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

notImplemented :: MonadIO m => m a
notImplemented = UIO.throwIO $ err500 { errBody = "Not implemented" }

getCourses
    :: EducatorWorkMode m
    => Maybe Bool -> m [Course]
getCourses _ = notImplemented

getCourse
    :: EducatorWorkMode m
    => Core.Course -> m Course
getCourse _ = notImplemented

getAssignments
    :: EducatorWorkMode m
    => Maybe Core.Course -> Maybe Core.DocumentType -> Maybe Bool
    -> m [Assignment]
getAssignments _ _ _ = notImplemented

getAssignment
    :: EducatorWorkMode m
    => Hash Core.Assignment -> m Assignment
getAssignment _ = notImplemented

getSubmissions
    :: EducatorWorkMode m
    => Maybe Core.Course -> Maybe (Hash Core.Assignment) -> Maybe Core.DocumentType
    -> m [Submission]
getSubmissions _ _ _ = notImplemented

getSubmission
    :: EducatorWorkMode m
    => Hash Core.Submission
    -> m Submission
getSubmission _ = notImplemented

makeSubmission
    :: EducatorWorkMode m
    => Core.SignedSubmission -> m Submission
makeSubmission _ = notImplemented

deleteSubmission
    :: EducatorWorkMode m
    => Hash Core.Submission
    -> m ()
deleteSubmission _ = notImplemented

getProofs
    :: EducatorWorkMode m
    => Maybe UTCTime -> m [BlkProof]
getProofs _ = notImplemented

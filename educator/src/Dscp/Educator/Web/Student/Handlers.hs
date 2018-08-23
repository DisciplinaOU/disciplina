-- | Student API handlers

module Dscp.Educator.Web.Student.Handlers
       ( studentApiHandlers
       , convertStudentApiHandler
       ) where

import Servant (Handler, throwError)

import Dscp.Core (Student)
import Dscp.DB.SQLite (sqlTransaction)
import Dscp.Educator.Launcher
import Dscp.Educator.Web.Student.API
import Dscp.Educator.Web.Student.Error
import Dscp.Educator.Web.Student.Logic
import Dscp.Educator.Web.Student.Queries
import Dscp.Rio

type StudentApiWorkMode m =
    ( MonadStudentAPIQuery m
    )

studentApiHandlers
    :: forall m. StudentApiWorkMode m
    => Student -> StudentApiHandlers m
studentApiHandlers student =
    StudentApiEndpoints
    { sGetCourses = \isEnrolledF ->
        sqlTransaction $ studentGetCourses student isEnrolledF

    , sGetCourse = \course ->
        studentGetCourse student course

    , sGetAssignments = \courseIdF docTypeF isFinalF ->
        sqlTransaction $ studentGetAssignments student courseIdF docTypeF isFinalF

    , sGetAssignment = \assignH ->
        sqlTransaction $ studentGetAssignment student assignH

    , sGetSubmissions = \courseIdF assignHF docTypeF ->
        sqlTransaction $ studentGetSubmissions student courseIdF assignHF docTypeF

    , sGetSubmission = \subH ->
        sqlTransaction $ studentGetSubmission student subH

    , sMakeSubmission = \newSub ->
        studentMakeSubmissionVerified student newSub

    , sDeleteSubmission = \subH ->
        sqlTransaction $ studentDeleteSubmission student subH

    , sGetProofs = \sinceF ->
        sqlTransaction $ studentGetProofs student sinceF
    }

convertStudentApiHandler
    :: EducatorContext
    -> EducatorRealMode a
    -> Handler a
convertStudentApiHandler ctx handler =
    liftIO (runRIO ctx handler)
        `catch` (throwError . toServantErr)
        `catchAny` (throwError . unexpectedToServantErr)

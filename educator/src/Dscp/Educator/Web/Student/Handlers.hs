-- | Student API handlers

module Dscp.Educator.Web.Student.Handlers
       ( studentApiHandlers
       , convertStudentApiHandler
       ) where

import Data.Default (def)
import Servant (Handler, throwError)
import UnliftIO (UnliftIO (..))

import Dscp.Core (Student)
import Dscp.DB.SQLite (sqlTransaction)
import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Student.API
import Dscp.Educator.Web.Student.Error
import Dscp.Educator.Web.Student.Logic
import Dscp.Educator.Web.Student.Queries
import Dscp.Educator.Web.Types

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

    , sGetAssignments = \afCourse afDocType afIsFinal ->
        sqlTransaction $
        commonGetAssignments StudentCase student
            def{ afCourse, afDocType, afIsFinal }

    , sGetAssignment = \assignH ->
        sqlTransaction $ studentGetAssignment student assignH

    , sGetSubmissions = \sfCourse sfAssignmentHash sfDocType ->
        commonGetSubmissions StudentCase
            def{ sfStudent = Just student, sfCourse, sfAssignmentHash, sfDocType }

    , sGetSubmission = \subH ->
        sqlTransaction $ studentGetSubmission student subH

    , sMakeSubmission = \newSub ->
        studentMakeSubmissionVerified student newSub

    , sDeleteSubmission = \subH ->
        sqlTransaction $ commonDeleteSubmission subH (Just student)

    , sGetProofs = \pfSince ->
        sqlTransaction $ commonGetProofs student def{ pfSince }
    }

convertStudentApiHandler
    :: UnliftIO m
    -> m a
    -> Handler a
convertStudentApiHandler (UnliftIO unliftIO) handler =
    liftIO (unliftIO handler)
        `catch` (throwError . toServantErr)
        `catchAny` (throwError . unexpectedToServantErr)

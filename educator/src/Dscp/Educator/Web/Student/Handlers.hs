-- | Student API handlers

module Dscp.Educator.Web.Student.Handlers
       ( studentApiHandlers
       , convertStudentApiHandler
       ) where

import Data.Default (def)
import Servant (Handler, throwError)
import UnliftIO (UnliftIO (..))

import Dscp.Core (Student)
import Dscp.DB.SQLite
import Dscp.Educator.Web.Logic
import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Student.API
import Dscp.Educator.Web.Student.Error
import Dscp.Educator.Web.Student.Logic
import Dscp.Educator.Web.Student.Queries
import Dscp.Educator.Web.Types

studentApiHandlers
    :: forall m ctx. MonadEducatorWeb ctx m
    => Student -> StudentApiHandlers m
studentApiHandlers student =
    StudentApiEndpoints
    { sGetCourses = \isEnrolledF ->
        transactR $ studentGetCourses student isEnrolledF

    , sGetCourse = \course ->
        transactR $ studentGetCourse student course

    , sGetAssignments = \afCourse afDocType afIsFinal ->
        transactR $
            commonGetAssignments StudentCase
                def{ afCourse, afStudent = Just student, afDocType, afIsFinal }

    , sGetAssignment = \assignH ->
        transactR $ studentGetAssignment student assignH

    , sGetSubmissions = \sfCourse sfAssignmentHash sfDocType ->
        invoke $
        commonGetSubmissions StudentCase
            def{ sfStudent = Just student, sfCourse, sfAssignmentHash, sfDocType }

    , sGetSubmission = \subH ->
        invoke $ studentGetSubmission student subH

    , sMakeSubmission = \newSub ->
        studentMakeSubmissionVerified student newSub

    , sDeleteSubmission = \subH ->
        transactW $ commonDeleteSubmission subH (Just student)

    , sGetProofs = \pfSince ->
        transactR $ commonGetProofs def{ pfSince, pfStudent = Just student }
    }

convertStudentApiHandler
    :: UnliftIO m
    -> m a
    -> Handler a
convertStudentApiHandler (UnliftIO unliftIO) handler =
    liftIO (unliftIO handler)
        `catch` (throwError . toServantErr)
        `catchAny` (throwError . unexpectedToServantErr)

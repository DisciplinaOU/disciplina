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
        transact $ studentGetCourses student isEnrolledF

    , sGetCourse = \course ->
        transact $ studentGetCourse student course

    , sGetAssignments = \afCourse afDocType afIsFinal ->
        transact $
            commonGetAssignments StudentCase student
                def{ afCourse, afDocType, afIsFinal }

    , sGetAssignment = \assignH ->
        transact $ studentGetAssignment student assignH

    , sGetSubmissions = \sfCourse sfAssignmentHash sfDocType ->
        invoke $
        commonGetSubmissions StudentCase
            def{ sfStudent = Just student, sfCourse, sfAssignmentHash, sfDocType }

    , sGetSubmission = \subH ->
        invoke $ studentGetSubmission student subH

    , sMakeSubmission = \newSub ->
        studentMakeSubmissionVerified student newSub

    , sDeleteSubmission = \subH ->
        transact $ commonDeleteSubmission subH (Just student)

    , sGetProofs = \pfSince ->
        transact $ commonGetProofs student def{ pfSince }
    }

convertStudentApiHandler
    :: UnliftIO m
    -> m a
    -> Handler a
convertStudentApiHandler (UnliftIO unliftIO) handler =
    liftIO (unliftIO handler)
        `catch` (throwError . toServantErr)
        `catchAny` (throwError . unexpectedToServantErr)

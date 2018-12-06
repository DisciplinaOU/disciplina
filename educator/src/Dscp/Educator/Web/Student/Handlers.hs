-- | Student API handlers

module Dscp.Educator.Web.Student.Handlers
       ( studentApiHandlers
       , convertStudentApiHandler
       ) where

import Data.Default (def)
import Servant (Handler)
import UnliftIO (UnliftIO (..))

import Dscp.Core (Student)
import Dscp.DB.SQLite
import Dscp.Educator.DB
import Dscp.Educator.Web.Logic
import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Student.API
import Dscp.Educator.Web.Student.Error
import Dscp.Educator.Web.Student.Logic
import Dscp.Educator.Web.Student.Queries
import Dscp.Educator.Web.Types
import Dscp.Web.Class

studentApiHandlers
    :: forall m ctx. MonadEducatorWeb ctx m
    => Student -> StudentApiHandlers m
studentApiHandlers student =
    StudentApiEndpoints
    {
      -- Courses

      sGetCourses = \isEnrolledF _onlyCount ->
        transactR $ studentGetCourses student isEnrolledF

    , sGetCourse = \course ->
        transactR $ studentGetCourse student course

      -- Assignments

    , sGetAssignments = \afCourse afDocType afIsFinal _onlyCount ->
        transactR $
            studentGetAssignments student
                def{ afCourse, afDocType, afIsFinal }

    , sGetAssignment = \assignH ->
        transactR $ studentGetAssignment student assignH

      -- Submissions`

    , sGetSubmissions = \sfCourse sfAssignmentHash sfDocType _onlyCount ->
        invoke $
        studentGetSubmissions student
            def{ sfCourse, sfAssignmentHash, sfDocType }

    , sAddSubmission = \newSub ->
        studentMakeSubmissionVerified student newSub

    , sGetSubmission = \subH ->
        invoke $ studentGetSubmission student subH

    , sDeleteSubmission = \subH ->
        transactW $ commonDeleteSubmission subH (Just student)

      -- Proofs

    , sGetProofs = \pfSince _onlyCount ->
        transactR $ commonGetProofs def{ pfSince, pfStudent = Just student }
    }

convertStudentApiHandler
    :: UnliftIO m
    -> m a
    -> Handler a
convertStudentApiHandler (UnliftIO unliftIO) handler =
    processServerErrors @StudentAPIError (unliftIO handler)

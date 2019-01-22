-- | Student API handlers

module Dscp.Educator.Web.Student.Handlers
       ( studentApiHandlers
       , convertStudentApiHandler
       ) where

import Data.Default (def)
import Servant (Handler)
import UnliftIO (UnliftIO (..))

import Dscp.Core (Student)
import Dscp.DB.SQL
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

      sGetCourses = \isEnrolledF _onlyCount sorting ->
        transact $ studentGetCourses student isEnrolledF sorting

    , sGetCourse = \course ->
        transact $ studentGetCourse student course

      -- Assignments

    , sGetAssignments = \afCourse afDocType afIsFinal _onlyCount sorting ->
        transact $
            studentGetAssignments student
                def{ afCourse, afDocType, afIsFinal }
                sorting

    , sGetAssignment = \assignH ->
        transact $ studentGetAssignment student assignH

      -- Submissions`

    , sGetSubmissions = \sfCourse sfAssignmentHash sfDocType _onlyCount sorting ->
        invoke $
        studentGetSubmissions student
            def{ sfCourse, sfAssignmentHash, sfDocType }
            sorting

    , sAddSubmission = \newSub ->
        studentMakeSubmissionVerified student newSub

    , sGetSubmission = \subH ->
        invoke $ studentGetSubmission student subH

    , sDeleteSubmission = \subH ->
        invoke $ commonDeleteSubmission subH (Just student)

      -- Proofs

    , sGetProofs = \pfSince _onlyCount ->
        transact $ commonGetProofs def{ pfSince, pfStudent = Just student }
    }

convertStudentApiHandler
    :: UnliftIO m
    -> m a
    -> Handler a
convertStudentApiHandler (UnliftIO unliftIO) handler =
    processServerErrors @StudentAPIError (unliftIO handler)

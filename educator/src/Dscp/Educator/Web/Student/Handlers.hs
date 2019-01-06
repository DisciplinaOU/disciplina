-- | Student API handlers

module Dscp.Educator.Web.Student.Handlers
       ( studentApiHandlers
       , convertStudentApiHandler
       ) where

import Servant (Handler)
import UnliftIO (UnliftIO (..))

import Dscp.Core (Student)
import Dscp.DB.SQL
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

      sGetCourses = \_onlyCount filters sorting pagination ->
        transact $ studentGetCourses student filters sorting pagination

    , sGetCourse = \course ->
        transact $ studentGetCourse student course

      -- Assignments

    , sGetAssignments = \_onlyCount filters sorting pagination ->
        transact $ studentGetAssignments student filters sorting pagination

    , sGetAssignment = \assignH ->
        transact $ studentGetAssignment student assignH

      -- Submissions

    , sGetSubmissions = \_onlyCount filters sorting pagination ->
        invoke $ studentGetSubmissions student filters sorting pagination

    , sAddSubmission = \newSub ->
        studentMakeSubmissionVerified student newSub

    , sGetSubmission = \subH ->
        invoke $ studentGetSubmission student subH

    , sDeleteSubmission = \subH ->
        invoke $ commonDeleteSubmission subH (Just student)

      -- Proofs

    , sGetProofs = \_onlyCount filters ->
        transact $ commonGetProofs filters
    }

convertStudentApiHandler
    :: UnliftIO m
    -> m a
    -> Handler a
convertStudentApiHandler (UnliftIO unliftIO) handler =
    processServerErrors @StudentAPIError (unliftIO handler)

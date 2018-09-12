-- | Educator API handlers

module Dscp.Educator.Web.Educator.Handlers
       ( educatorApiHandlers
       , convertEducatorApiHandler
       ) where

import Data.Default (def)
import Servant (Handler, throwError)
import UnliftIO (UnliftIO (..))

import Dscp.DB.SQLite
import Dscp.Educator.Web.Educator.API
import Dscp.Educator.Web.Educator.Error
import Dscp.Educator.Web.Educator.Logic
import Dscp.Educator.Web.Educator.Queries
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Logic
import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Types

educatorApiHandlers
    :: forall m ctx. MonadEducatorWeb ctx m
    => EducatorApiHandlers m
educatorApiHandlers =
    EducatorApiEndpoints
    {
      -- Students

      eNewStudent =
        void . invoke ... createStudent

    , eRemoveStudent =
        invoke ... educatorRemoveStudent

    , eGetStudents =
        invoke ... educatorGetStudents

      -- Courses

    , eAddCourse = \(NewCourse cid desc subjects) ->
        void . transact $ createCourse CourseDetails
            { cdCourseId = cid
            , cdDesc = desc ?: ""
            , cdSubjects = subjects ?: []
            }

    , eGetCourses =
        invoke $ educatorGetCourses Nothing

    , eEnrollStudentToCourse = \student (EnrollStudentToCourse course) ->
        transact $ enrollStudentToCourse student course

    , eGetStudentCourses = \student ->
        invoke $ educatorGetCourses (Just student)

      -- Assignments

    , eAddCourseAssignment = \_autoAssign na -> do
        void . transact $ createAssignment (requestToAssignment na)
        -- TODO [DSCP-176]: consider autoassign

    , eGetStudentAssignments = \student ->
        transact $ commonGetAssignments EducatorCase student def

    , eAssignToStudent =
        transact ... setStudentAssignment

    , eUnassignFromStudent =
        invoke ... educatorUnassignFromStudent

    , eGetStudentCourseAssignments = \student course afIsFinal ->
        transact $
          commonGetAssignments EducatorCase student
              def{ afCourse = Just course, afIsFinal }

      -- Submissions

    , eGetSubmission =
        invoke ... educatorGetSubmission

    , eDeleteSubmission = \submissionH ->
        transact $ commonDeleteSubmission submissionH Nothing

    , eGetSubmissions =
        invoke $ commonGetSubmissions EducatorCase def

    , eGetStudentSubmissions = \student ->
        invoke $ commonGetSubmissions EducatorCase
            def{ sfStudent = Just student }

    , eGetStudentAssignmentSubmissions = \student assignH ->
        invoke $ commonGetSubmissions EducatorCase
            def{ sfStudent = Just student, sfAssignmentHash = Just assignH }

    , eGetStudentCourseSubmissions = \student course ->
        invoke $ commonGetSubmissions EducatorCase
            def{ sfStudent = Just student, sfCourse = Just course }

      -- Grades

    , ePostGrade = \(NewGrade subH grade) ->
        invoke $ educatorPostGrade subH grade

    , eGetGrades =
        invoke $ educatorGetGrades Nothing Nothing Nothing

    , eGetStudentGrades = \student ->
        invoke $ educatorGetGrades (Just student) Nothing Nothing

    , eGetStudentCourseGrades = \student course isFinalF ->
        invoke $ educatorGetGrades (Just student) (Just course) isFinalF

      -- Proofs

    , eGetStudentProofs = \student ->
        transact $ commonGetProofs student def

    , eGetStudentCourseProofs = \student courseF ->
        transact $ commonGetProofs student
            def{ pfCourse = Just courseF }
    }

convertEducatorApiHandler
    :: UnliftIO m
    -> m a
    -> Handler a
convertEducatorApiHandler (UnliftIO unliftIO) handler =
    liftIO (unliftIO handler)
        `catch` (throwError . toServantErr)
        `catchAny` (throwError . unexpectedToServantErr)

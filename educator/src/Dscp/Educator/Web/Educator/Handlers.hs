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
import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Types

educatorApiHandlers
    :: forall m. EducatorApiWorkMode m
    => EducatorApiHandlers m
educatorApiHandlers =
    EducatorApiEndpoints
    {
      -- * Students

      eNewStudent =
        void ... createStudent

    , eRemoveStudent =
        educatorRemoveStudent

    , eGetStudents =
        educatorGetStudents

      -- * Courses

    , eAddCourse = \(NewCourse cid desc subjects) ->
        void $ createCourse CourseDetails
            { cdCourseId = cid
            , cdDesc = desc ?: ""
            , cdSubjects = subjects ?: []
            }

    , eGetCourses =
        educatorGetCourses Nothing

    , eEnrollStudentToCourse = \student (EnrollStudentToCourse course) ->
        enrollStudentToCourse student course

    , eGetStudentCourses = \student ->
        educatorGetCourses (Just student)

      -- * Assignments

    , eAddCourseAssignment = \_autoAssign na -> do
        void $ createAssignment (requestToAssignment na)
        error "Auto assign (in transaction!)"

    , eGetStudentAssignments = \student ->
        sqlTransaction $ commonGetAssignments EducatorCase student def

    , eAssignToStudent =
        setStudentAssignment

    , eUnassignFromStudent =
        educatorUnassignFromStudent

    , eGetStudentCourseAssignments = \student course afIsFinal ->
        sqlTransaction $
        commonGetAssignments EducatorCase student
            def{ afCourse = Just course, afIsFinal }

      -- * Submissions

    , eGetSubmission =
        educatorGetSubmission

    , eDeleteSubmission = \submissionH ->
        sqlTransaction $ commonDeleteSubmission submissionH Nothing

    , eGetSubmissions =
        commonGetSubmissions EducatorCase def

    , eGetStudentSubmissions = \student ->
        commonGetSubmissions EducatorCase
            def{ sfStudent = Just student }

    , eGetStudentAssignmentSubmissions = \student assignH ->
        commonGetSubmissions EducatorCase
            def{ sfStudent = Just student, sfAssignmentHash = Just assignH }

    , eGetStudentCourseSubmissions = \student course ->
        commonGetSubmissions EducatorCase
            def{ sfStudent = Just student, sfCourse = Just course }

      -- * Grades

    , ePostGrade = \(NewGrade subH grade) ->
        educatorPostGrade subH grade

    , eGetGrades =
        educatorGetGrades Nothing Nothing Nothing

    , eGetStudentGrades = \student ->
        educatorGetGrades (Just student) Nothing Nothing

    , eGetStudentCourseGrades = \student course isFinalF ->
        educatorGetGrades (Just student) (Just course) isFinalF

      -- * Proofs

    , eGetStudentProofs = \student ->
        sqlTransaction $ commonGetProofs student def

    , eGetStudentCourseProofs = \student courseF ->
        sqlTransaction $ commonGetProofs student
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

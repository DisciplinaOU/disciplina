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

      eNewStudent = \(NewStudent student) ->
        void . invoke $ createStudent student

    , eRemoveStudent =
        invoke ... educatorRemoveStudent

    , eGetStudents =
        invoke ... educatorGetStudents

      -- Courses

    , eAddCourse = \(NewCourse mcid desc subjects) ->
        transactW $ createCourse CourseDetails
            { cdCourseId = mcid
            , cdDesc = desc
            , cdSubjects = subjects
            }

    , eGetCourses =
        invoke ... educatorGetCourses

    , eEnrollStudentToCourse = \student (EnrollStudentToCourse course) ->
        transactW $ enrollStudentToCourse student course

      -- Assignments

    , eAddCourseAssignment = \_autoAssign na -> do
        void . transactW $ createAssignment (requestToAssignment na)
        -- TODO [DSCP-176]: consider autoassign

    , eGetAssignments = \afCourse afStudent afIsFinal ->
        transactR $ commonGetAssignments EducatorCase
            def{ afCourse, afStudent, afIsFinal }

    , eAssignToStudent = \student (AssignToStudent assignmentHash) ->
        transactW $ setStudentAssignment student assignmentHash

    , eUnassignFromStudent =
        invoke ... educatorUnassignFromStudent

      -- Submissions

    , eGetSubmission =
        invoke ... educatorGetSubmission

    , eDeleteSubmission = \submissionH ->
        transactW $ commonDeleteSubmission submissionH Nothing

    , eGetSubmissions = \sfCourse sfStudent sfAssignmentHash ->
        invoke $ commonGetSubmissions EducatorCase
            def{ sfCourse, sfStudent, sfAssignmentHash }

      -- Grades

    , ePostGrade = \(NewGrade subH grade) ->
        invoke $ educatorPostGrade subH grade

    , eGetGrades = \course student assignment isFinalF ->
        invoke $ educatorGetGrades course student assignment isFinalF

      -- Proofs

    , eGetProofs = \pfCourse pfStudent pfAssignment ->
        transactR $ commonGetProofs
            def{ pfCourse, pfStudent, pfAssignment }
    }

convertEducatorApiHandler
    :: UnliftIO m
    -> m a
    -> Handler a
convertEducatorApiHandler (UnliftIO unliftIO) handler =
    liftIO (unliftIO handler)
        `catch` (throwError . toServantErr)
        `catchAny` (throwError . unexpectedToServantErr)

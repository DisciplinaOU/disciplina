-- | Educator API handlers

module Dscp.Educator.Web.Educator.Handlers
       ( educatorApiHandlers
       , convertEducatorApiHandler
       ) where

import Data.Default (def)
import Servant (Handler)
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
import Dscp.Web.Class

educatorApiHandlers
    :: forall m ctx. MonadEducatorWeb ctx m
    => EducatorApiHandlers m
educatorApiHandlers =
    EducatorApiEndpoints
    {
      -- Students

      eGetStudents =
        invoke ... educatorGetStudents

    , eAddStudent = \(NewStudent student) ->
        void . invoke $ createStudent student

    , eDeleteStudent =
        invoke ... educatorRemoveStudent

    , eAddStudentCourse = \student (NewStudentCourse course) ->
        transactW $ enrollStudentToCourse student course

    , eAddStudentAssignment = \student (NewStudentAssignment assignmentHash) ->
        transactW $ setStudentAssignment student assignmentHash

    , eDeleteStudentAssignment =
        invoke ... educatorUnassignFromStudent

      -- Courses

    , eGetCourses =
        invoke ... educatorGetCourses

    , eAddCourse = \(NewCourse mcid desc subjects) ->
        transactW $ createCourse CourseDetails
            { cdCourseId = mcid
            , cdDesc = desc
            , cdSubjects = subjects
            }

    , eGetCourse =
        invoke ... educatorGetCourse

      -- Assignments

    , eGetAssignments = \afCourse afStudent afIsFinal ->
        transactR $ commonGetAssignments EducatorCase
            def{ afCourse, afStudent, afIsFinal }

    , eAddAssignment = \_autoAssign na -> do
        void . transactW $ createAssignment (requestToAssignment na)
        -- TODO [DSCP-176]: consider autoassign

      -- Submissions

    , eGetSubmissions = \sfCourse sfStudent sfAssignmentHash ->
        invoke $ commonGetSubmissions EducatorCase
            def{ sfCourse, sfStudent, sfAssignmentHash }

    , eGetSubmission =
        invoke ... educatorGetSubmission

    , eDeleteSubmission = \submissionH ->
        transactW $ commonDeleteSubmission submissionH Nothing

      -- Grades

    , eGetGrades = \course student assignment isFinalF ->
        invoke $ educatorGetGrades course student assignment isFinalF

    , eAddGrade = \(NewGrade subH grade) ->
        invoke $ educatorPostGrade subH grade

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
    processServerErrors @APIError (unliftIO handler)

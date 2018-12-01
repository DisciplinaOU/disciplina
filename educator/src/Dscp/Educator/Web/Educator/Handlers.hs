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
      eGetStatus =
        getEducatorStatus

      -- Students

    , eGetStudents = \mCourse _mIsEnrolled _onlyCount ->
        invoke $ educatorGetStudents mCourse

    , eAddStudent = \(NewStudent student) ->
        invoke $ void $ createStudent student

    , eDeleteStudent = \studentId ->
        invoke $ educatorRemoveStudent studentId

    , eAddStudentCourse = \student (NewStudentCourse course) ->
        transactW $ enrollStudentToCourse student course

    , eAddStudentAssignment = \student (NewStudentAssignment assignmentHash) ->
        transactW $ setStudentAssignment student assignmentHash

    , eDeleteStudentAssignment = \student assignmentId ->
        invoke $ educatorUnassignFromStudent student assignmentId

      -- Courses

    , eGetCourses = \mStudent _onlyCount ->
        invoke $ educatorGetCourses mStudent

    , eAddCourse = \(NewCourse mcid desc subjects) ->
        transactW $ createCourse CourseDetails
            { cdCourseId = mcid
            , cdDesc = desc
            , cdSubjects = subjects
            }

    , eGetCourse = \courseId ->
        transactR $ educatorGetCourse courseId

      -- Assignments

    , eGetAssignments = \afCourse afStudent afIsFinal _afSince _afOnlyCount ->
        invoke $ educatorGetAssignments
            def{ afCourse, afStudent, afIsFinal }

    , eAddAssignment = \_autoAssign na -> do
        transactW $ void $ createAssignment (requestToAssignment na)
        -- TODO [DSCP-176]: consider autoassign

      -- Submissions

    , eGetSubmissions = \sfCourse sfStudent sfAssignmentHash _sfIsGraded _sfSince _sfOnlyCount ->
        invoke $ educatorGetSubmissions
            def{ sfCourse, sfStudent, sfAssignmentHash }

    , eGetSubmission = \submissionHash ->
        invoke $ educatorGetSubmission submissionHash

    , eDeleteSubmission = \submissionH ->
        transactW $ commonDeleteSubmission submissionH Nothing

      -- Grades

    , eGetGrades = \course student assignment isFinalF _since _onlyCount ->
        invoke $ educatorGetGrades course student assignment isFinalF

    , eAddGrade = \(NewGrade subH grade) ->
        transactW $ educatorPostGrade subH grade

      -- Proofs

    , eGetProofs = \pfCourse pfStudent pfAssignment _pfOnlyCount ->
        transactR $ commonGetProofs
            def{ pfCourse, pfStudent, pfAssignment }
    }

convertEducatorApiHandler
    :: UnliftIO m
    -> m a
    -> Handler a
convertEducatorApiHandler (UnliftIO unliftIO) handler =
    processServerErrors @APIError (unliftIO handler)

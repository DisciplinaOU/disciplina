-- | Educator API handlers

module Dscp.Educator.Web.Educator.Handlers
       ( educatorApiHandlers
       , convertEducatorApiHandler
       ) where

import Data.Default (def)
import Servant (Handler)
import UnliftIO (UnliftIO (..))

import Dscp.DB.SQL
import Dscp.Educator.DB
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
        void . invoke $ createStudent student

    , eDeleteStudent =
        invoke ... educatorRemoveStudent

    , eAddStudentCourse = \student (NewStudentCourse course) ->
        transact $ enrollStudentToCourse student course

    , eAddStudentAssignment = \student (NewStudentAssignment assignmentHash) ->
        transact $ setStudentAssignment student assignmentHash

    , eDeleteStudentAssignment =
        invoke ... educatorUnassignFromStudent

      -- Courses

    , eGetCourses = \mStudent _onlyCount ->
        invoke $ educatorGetCourses mStudent

    , eAddCourse = \(NewCourse mcid desc subjects) ->
        transact $ createCourse CourseDetails
            { cdCourseId = mcid
            , cdDesc = desc
            , cdSubjects = subjects
            }

    , eGetCourse =
        invoke ... educatorGetCourse

      -- Assignments

    , eGetAssignments = \afCourse afStudent afIsFinal _afSince _afOnlyCount ->
        invoke $ educatorGetAssignments
            def{ afCourse, afStudent, afIsFinal }

    , eAddAssignment = \_autoAssign na -> do
        void . transact $ createAssignment (requestToAssignment na)
        -- TODO [DSCP-176]: consider autoassign

      -- Submissions

    , eGetSubmissions = \sfCourse sfStudent sfAssignmentHash _sfIsGraded _sfSince _sfOnlyCount ->
        invoke $ educatorGetSubmissions
            def{ sfCourse, sfStudent, sfAssignmentHash }

    , eGetSubmission =
        invoke ... educatorGetSubmission

    , eDeleteSubmission = \submissionH ->
        invoke $ commonDeleteSubmission submissionH Nothing

      -- Grades

    , eGetGrades = \course student assignment isFinalF _since _onlyCount ->
        invoke $ educatorGetGrades course student assignment isFinalF

    , eAddGrade = \(NewGrade subH grade) ->
        transact $ educatorPostGrade subH grade

      -- Proofs

    , eGetProofs = \pfCourse pfStudent pfAssignment _pfOnlyCount ->
        transact $ commonGetProofs
            def{ pfCourse, pfStudent, pfAssignment }
    }

convertEducatorApiHandler
    :: UnliftIO m
    -> m a
    -> Handler a
convertEducatorApiHandler (UnliftIO unliftIO) handler =
    processServerErrors @EducatorAPIError (unliftIO handler)

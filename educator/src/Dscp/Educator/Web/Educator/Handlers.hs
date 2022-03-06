-- | Educator API handlers

module Dscp.Educator.Web.Educator.Handlers
       ( educatorApiHandlers
       , convertEducatorApiHandler
       ) where

import Universum
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

    , eGetStudents = \mCourse _mIsEnrolled onlyCount pagination ->
        fmap (mkCountedList onlyCount) $ invoke $ educatorGetStudents mCourse pagination

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

    , eGetCourses = \mStudent onlyCount pagination ->
        fmap (mkCountedList onlyCount) $ invoke $ educatorGetCourses mStudent pagination

    , eAddCourse = \(NewCourse desc subjects) ->
        transact $ createCourse CourseDetails
            { cdCourseId = Nothing
            , cdDesc = desc
            , cdSubjects = subjects
            }

    , eGetCourse =
        invoke ... educatorGetCourse

      -- Assignments

    , eGetAssignments = \afCourse afStudent afIsFinal _afSince afOnlyCount pagination ->
            fmap (mkCountedList afOnlyCount) $ invoke $
            educatorGetAssignments def{ afCourse, afStudent, afIsFinal } pagination

    , eAddAssignment = \_autoAssign na -> do
        void . transact $ createAssignment (requestToAssignment na)
        -- TODO [DSCP-176]: consider autoassign

      -- Submissions

    , eGetSubmissions = \sfCourse sfStudent sfAssignmentHash _sfIsGraded _sfSince
                         sfOnlyCount pagination ->
            fmap (mkCountedList sfOnlyCount) $ invoke $
            educatorGetSubmissions
                def{ sfCourse, sfStudent, sfAssignmentHash }
                pagination

    , eGetSubmission =
        invoke ... educatorGetSubmission

    , eDeleteSubmission = \submissionH ->
        invoke $ commonDeleteSubmission submissionH Nothing

      -- Grades

    , eGetGrades = \course student assignment isFinalF _since onlyCount ->
            fmap (mkCountedList onlyCount) $ invoke $
            educatorGetGrades course student assignment isFinalF

    , eAddGrade = \(NewGrade subH grade) ->
            transact $ educatorPostGrade subH grade

      -- Proofs

    , eGetProofs = \pfCourse pfStudent pfAssignment pfOnlyCount ->
            fmap (mkCountedList pfOnlyCount) $ transact $
            commonGetProofs def{ pfCourse, pfStudent, pfAssignment }

      -- Certificates
    , eGetCertificates = \sorting pagination onlyCount ->
            invoke $ fmap (mkCountedList onlyCount) $
            educatorGetCertificates sorting pagination

    , eGetCertificate =
            invoke ... educatorGetCertificate

    , eAddCertificate = \cert@(CertificateFullInfo meta _) -> do
            void $ educatorAddCertificate cert
            pure $ mkCertificate meta
    }

convertEducatorApiHandler
    :: UnliftIO m
    -> m a
    -> Handler a
convertEducatorApiHandler (UnliftIO unliftIO) handler =
    processServerErrors @EducatorAPIError (unliftIO handler)

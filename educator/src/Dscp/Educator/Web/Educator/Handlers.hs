-- | Educator API handlers

module Dscp.Educator.Web.Educator.Handlers
       ( educatorApiHandlers
       , convertEducatorApiHandler
       ) where

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

    , eGetStudents = \onlyCount filters pagination ->
        fmap (mkCountedList onlyCount) $ invoke $ educatorGetStudents filters pagination

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

    , eGetCourses = \onlyCount filters pagination ->
        fmap (mkCountedList onlyCount) $ invoke $ educatorGetCourses filters pagination

    , eAddCourse = \(NewCourse desc subjects) ->
        transact $ createCourse CourseDetails
            { cdCourseId = Nothing
            , cdDesc = desc
            , cdSubjects = subjects
            }

    , eGetCourse =
        invoke ... educatorGetCourse

      -- Assignments

    , eGetAssignments = \afOnlyCount filters pagination ->
            fmap (mkCountedList afOnlyCount) $ invoke $
            educatorGetAssignments filters pagination

    , eAddAssignment = \_autoAssign na -> do
        void . transact $ createAssignment (requestToAssignment na)
        -- TODO [DSCP-176]: consider autoassign

      -- Submissions

    , eGetSubmissions = \sfOnlyCount filters pagination ->
            fmap (mkCountedList sfOnlyCount) $ invoke $
            educatorGetSubmissions filters pagination

    , eGetSubmission =
        invoke ... educatorGetSubmission

    , eDeleteSubmission = \submissionH ->
        invoke $ commonDeleteSubmission submissionH Nothing

      -- Grades

    , eGetGrades = \onlyCount filters ->
            fmap (mkCountedList onlyCount) $ invoke $
            educatorGetGrades filters

    , eAddGrade = \(NewGrade subH grade) ->
            transact $ educatorPostGrade subH grade

      -- Proofs

    , eGetProofs = \onlyCount filters ->
            fmap (mkCountedList onlyCount) $ transact $
            commonGetProofs filters

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

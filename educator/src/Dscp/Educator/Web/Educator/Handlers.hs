-- | Educator API handlers

module Dscp.Educator.Web.Educator.Handlers
       ( educatorApiHandlers
       , convertEducatorApiHandler
       ) where

import Servant (Handler, throwError)

import Dscp.Core.Arbitrary
import Dscp.Educator.Launcher
import Dscp.Educator.Web.Arbitrary
import Dscp.Educator.Web.Educator.API
import Dscp.Educator.Web.Educator.Arbitrary
import Dscp.Educator.Web.Educator.Error
import Dscp.Rio

type EducatorApiWorkMode m =
    ( Monad m
    )

educatorApiHandlers
    :: forall m. EducatorApiWorkMode m
    => EducatorApiHandlers m
educatorApiHandlers =
    EducatorApiEndpoints
    {
      -- * Students

      eNewStudent = \_student ->
        pass

    , eRemoveStudent = \_student ->
        pass

    , eGetStudents = \_courseF ->
        return [studentEx]

      -- * Courses

    , eAddCourse = \_newCourse ->
        pass

    , eGetCourses =
        return [educatorCourseInfoEx]

    , eEnrollStudentToCourse = \_student _course ->
        pass

    , eGetStudentCourses = \_student ->
        return [educatorCourseInfoEx]

      -- * Assignments

    , eAddCourseAssignment = \_newAssign _autoAssign ->
        pass

    , eGetStudentAssignments = \_student ->
        return [assignmentEx]

    , eAssignToStudent = \_student _assignH ->
        pass

    , eUnassignFromStudent = \_student _assignH ->
        pass

    , eGetStudentCourseAssignments = \_student _course _isFinalF ->
        return [assignmentEx]

      -- * Submissions

    , eGetSubmission = \_subH ->
        return [educatorSubmissionInfoEx]

    , eDeleteSubmission = \_subH ->
        pass

    , eGetSubmissions =
        return [educatorSubmissionInfoEx]

    , eGetStudentSubmissions = \_student ->
        return [educatorSubmissionInfoEx]

    , eGetStudentAssignmentSubmissions = \_student _assignH ->
        return [educatorSubmissionInfoEx]

    , eGetStudentCourseSubmissions = \_student _course ->
        return [educatorSubmissionInfoEx]

      -- * Grades

    , ePostGrade = \_newGrade ->
        pass

    , eGetGrades =
        return [gradeInfoEx]

    , eGetStudentGrades = \_student ->
        return [gradeInfoEx]

    , eGetStudentCourseGrades = \_student _course _isFinalF ->
        return [gradeInfoEx]

      -- * Proofs

    , eGetStudentProofs = \_student ->
        return [blkProofInfoEx]

    , eGetStudentCourseProofs = \_student _course ->
        return [blkProofInfoEx]
    }

convertEducatorApiHandler
    :: EducatorContext
    -> EducatorRealMode a
    -> Handler a
convertEducatorApiHandler ctx handler =
    liftIO (runRIO ctx handler)
        `catch` (throwError . toServantErr)
        `catchAny` (throwError . unexpectedToServantErr)

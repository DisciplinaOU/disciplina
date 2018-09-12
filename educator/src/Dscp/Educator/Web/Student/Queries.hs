{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TypeOperators             #-}

module Dscp.Educator.Web.Student.Queries
    ( module Dscp.Educator.Web.Student.Queries
    ) where

import Data.Coerce (coerce)
import Database.SQLite.Simple (Only (..), Query)
import Text.InterpolatedString.Perl6 (q)

import Dscp.Core
import Dscp.Crypto (Hash)
import Dscp.DB.SQLite
import qualified Dscp.DB.SQLite.Queries as Base
import Dscp.Util (Id, assertJust, listToMaybeWarn)

import Dscp.Educator.Web.Student.Types
import Dscp.Educator.Web.Types

studentIsCourseFinished
    :: MonadEducatorWebQuery m
    => Id Student -> Course -> DBT r m Bool
studentIsCourseFinished studentId courseId = do
    finalAssignmentsGrades <- query queryText (CourseFinal, courseId, studentId)
    return $ any isPositiveGrade finalAssignmentsGrades
  where
    queryText :: Query
    queryText = [q|
        select    grade
        from      Transactions
        left join Submissions
               on Transactions.submission_hash = Submissions.hash
        left join Assignments
               on Submissions.assignment_hash = Assignments.hash
        where     Assignments.type = ?
              and Assignments.course_id = ?
              and Submissions.student_addr = ?
    |]

studentGetCourse
    :: MonadEducatorWebQuery m
    => Id Student -> Course -> DBT WithinTx m CourseStudentInfo
studentGetCourse studentId courseId = do
    mcourse <-
        query queryText (Only courseId)
        >>= listToMaybeWarn "courses"
    Only mdesc <-
        pure mcourse `assertJust` AbsentError (CourseDomain courseId)

    ciIsEnrolled <- Base.isEnrolledTo studentId courseId
    ciSubjects <- Base.getCourseSubjects courseId
    ciIsFinished <- studentIsCourseFinished studentId courseId
    let ciDesc = fromMaybe "" mdesc
    return CourseStudentInfo{ ciId = courseId, .. }
  where
    queryText :: Query
    queryText = [q|
        select    desc
        from      Courses
        where     id = ?
    |]

studentGetCourses
    :: MonadEducatorWebQuery m
    => Id Student -> Maybe IsEnrolled -> DBT WithinTx m [CourseStudentInfo]
studentGetCourses studentId (coerce -> isEnrolledF) = do
    let (enrolledClause, enrolledParam) = case isEnrolledF of
            Just isEnrolled -> (mkEnrolledClause isEnrolled, oneParam studentId)
            Nothing         -> ("", mempty)

    courses <- query (queryText <> enrolledClause)
                      enrolledParam

    forM courses $ \(courseId, mdesc) -> do
        ciIsEnrolled <- Base.isEnrolledTo studentId courseId
        ciSubjects <- Base.getCourseSubjects courseId
        ciIsFinished <- studentIsCourseFinished studentId courseId
        let ciDesc = fromMaybe "" mdesc
        return CourseStudentInfo{ ciId = courseId, .. }
  where
    queryText :: Query
    queryText = [q|
        select distinct Courses.id, Courses.desc
        from            Courses
    |]
    mkEnrolledClause sign =
        "where " <> bool "not" "" sign <> [q| exists
        (
            select *
            from   StudentCourses
            where  StudentCourses.course_id = Courses.id
               and StudentCourses.student_addr = ?
        )
    |]

studentGetGrade
    :: MonadEducatorWebQuery m
    => Hash Submission -> DBT WithinTx m (Maybe GradeInfo)
studentGetGrade submissionH = do
    mgrade <-
        query queryText (Only submissionH)
        >>= listToMaybeWarn "last submission"
    forM mgrade $ \(giGrade, giTimestamp, giSubmissionHash, blkIdx) -> do
        let giHasProof = blkIdx /= TxInMempool
        return GradeInfo{..}
  where
    queryText :: Query
    queryText = [q|
        select    grade, time, submission_hash, idx
        from      Transactions
        where     submission_hash = ?
    |]

studentGetLastAssignmentSubmission
    :: MonadEducatorWebQuery m
    => Student
    -> Hash Assignment
    -> DBT WithinTx m (Maybe SubmissionStudentInfo)
studentGetLastAssignmentSubmission student assignH = do
    msubmission <-
        query queryText (assignH, student)
        >>= listToMaybeWarn "last submission"
    forM msubmission $ \(submissionH, siContentsHash, siAssignmentHash) -> do
        siGrade <- studentGetGrade submissionH
        return SubmissionStudentInfo{ siHash = submissionH, .. }
  where
    queryText :: Query
    queryText = [q|
        select    hash, contents_hash, assignment_hash
        from      Submissions
        where     assignment_hash = ?
              and student_addr = ?
        order by  creation_time desc
        limit     1
    |]

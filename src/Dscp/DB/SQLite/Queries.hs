{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.Queries where

import Control.Lens (to)

import Database.SQLite.Simple (Only (..), Query)
import Database.SQLite.Simple.ToRow (ToRow (..))

import Text.InterpolatedString.Perl6 (q)

import Dscp.Core.Serialise ()
import Dscp.Core.Types (Assignment (..), CourseId, Grade, SignedSubmission (..), StudentId,
                        Submission (..), aAssignment, aCourseId, aType, sAssignment, sStudentId,
                        sType, ssSubmission, ssWitness, swSig)
import Dscp.Crypto (Hash, PublicKey, hash)
import Dscp.DB.SQLite.Class
import Dscp.DB.SQLite.Instances ()
import Dscp.Educator.Txs (PrivateTx (..), PrivateTxId)

data DomainError
    = CourseDoesNotExist                  CourseId
    | StudentDoesNotExist                 StudentId
    | AssignmentDoesNotExist              (Hash Assignment)
    | StudentWasNotSubscribedOnAssignment StudentId (Hash Assignment)
    deriving (Show, Typeable)

instance Exception DomainError

type DBM m = MonadSQLiteDB m

getStudentCourses :: DBM m => StudentId -> m [CourseId]
getStudentCourses student =
    query getStudentCoursesQuery (Only student)
  where
    getStudentCoursesQuery :: Query
    getStudentCoursesQuery = [q|
        select  course_id
        from    StudentCourses
        where   student_addr = ?
    |]

enrollStudentToCourse :: DBM m => StudentId -> CourseId -> m ()
enrollStudentToCourse student course = do
    execute enrollStudentToCourseRequest (student, course)
  where
    enrollStudentToCourseRequest :: Query
    enrollStudentToCourseRequest = [q|
        insert into  StudentCourses
        values       (?, ?)
    |]

getStudentAssignments :: DBM m => StudentId -> CourseId -> m [Assignment]
getStudentAssignments student course = do
    query getStudentAssignmentsQuery (student, course)
  where
    getStudentAssignmentsQuery :: Query
    getStudentAssignmentsQuery = [q|
        select     course_id, student_addr, desc
        from       StudentAssignments
        left join  Assignments
               on  assignment_hash = Assignments.hash
        where      student_addr    = ?
               and course_id       = ?
    |]

submitAssignment :: DBM m => SignedSubmission -> m (Hash Submission)
submitAssignment = createSignedSubmission

createSignedSubmission :: DBM m => SignedSubmission -> m (Hash Submission)
createSignedSubmission sigSub = do
    let
        submission     = sigSub^.ssSubmission
        submissionSig  = sigSub^.ssWitness.swSig

        student        = submission^.sStudentId
        submissionHash = submission^.to hash
        submissionType = submission^.sType
        assignment     = submission^.sAssignment

        course         = assignment^.aCourseId
        assignmentHash = assignment^.to hash

    transaction $ do
        _ <- existsStudent student        `assert`     StudentDoesNotExist    student
        _ <- existsCourse  course         `assert`     CourseDoesNotExist     course
        _ <- getAssignment assignmentHash `assertJust` AssignmentDoesNotExist assignmentHash

        _ <- isAssignedToStudent student assignmentHash
            `assert` StudentWasNotSubscribedOnAssignment student assignmentHash

        execute generateSubmissionRequest
            ( submissionHash
            , student
            , assignmentHash
            , submissionType
            , submissionSig
            )

    return submissionHash
  where
    generateSubmissionRequest :: Query
    generateSubmissionRequest = [q|
        insert into  Submissions
        values       (?, ?, ?, ?, ?)
    |]

setStudentAssignment :: DBM m => StudentId -> Hash Assignment -> m (StudentId, Hash Assignment)
setStudentAssignment student assignment = do
    execute setStudentAssignmentRequest (student, assignment)
    return (student, assignment)
  where
    setStudentAssignmentRequest :: Query
    setStudentAssignmentRequest = [q|
        insert into  StudentAssignments
        values      (?, ?)
    |]

isAssignedToStudent :: DBM m => StudentId -> Hash Assignment -> m Bool
isAssignedToStudent student assignment = do
    exists getStudentAssignmentQuery (student, assignment)
  where
    getStudentAssignmentQuery :: Query
    getStudentAssignmentQuery = [q|
        select  1
        from    StudentAssignments
        where   student_addr = ?
           and  assignment_hash = ?
    |]

getGradesForCourseAssignments :: DBM m => StudentId -> CourseId -> m [PrivateTx]
getGradesForCourseAssignments student course = do
    query getGradesForCourseAssignmentsQuery (course, student)
  where
    getGradesForCourseAssignmentsQuery :: Query
    getGradesForCourseAssignmentsQuery = [q|
        select     Submissions.student_addr,
                   Submissions.contents_hash,
                   Assignments.course_id,
                   Assignments.contents_hash,
                   Assignments.desc,
                   Submissions.signature
                   grade
                   time

        from       Transactions

        left join  Submissions
               on  submission_hash = Submissions.hash

        left join  Assignments
               on  sssignment_hash = Assignments.hash
              and  Assignments.course_id = ?

        where      student_addr = ?
    |]

getStudentTransactions :: DBM m => PublicKey -> StudentId -> m [PrivateTx]
getStudentTransactions pk student = do
    map ($ pk) <$> query getStudentTransactionsQuery (Only student)
  where
    getStudentTransactionsQuery :: Query
    getStudentTransactionsQuery = [q|
        select     Submissions.student_addr,
                   Submissions.contents_hash,
                   Assignments.course_id,
                   Assignments.contents_hash,
                   Assignments.desc,
                   Submissions.signature
                   grade
                   time

        from       Transactions

        left join  Submissions
               on  submission_hash = Submissions.hash

        left join  Assignments
               on  sssignment_hash = Assignments.hash

        where      student_addr = ?
    |]

createCourse :: DBM m => CourseId -> Maybe Text -> m CourseId
createCourse course desc = do
    execute createCourseRequest (course, desc)
    return course
  where
    createCourseRequest = [q|
        insert into  Courses
        values       (?, ?)
    |]

assert :: DBM m => m Bool -> DomainError -> m ()
assert action message = do
    yes <- action

    unless yes $ do
        throwIO message

assertJust :: DBM m => m (Maybe a) -> DomainError -> m a
assertJust action message = do
    mb <- action

    whenNothing mb $ do
        throwIO message

existsCourse :: DBM m => CourseId -> m Bool
existsCourse course = do
    exists existsCourseQuery (Only course)
  where
    existsCourseQuery = [q|
        select  1
        from    Courses
        where   id = ?
    |]

existsStudent :: DBM m => StudentId -> m Bool
existsStudent student = do
    exists existsCourseQuery (Only student)
  where
    existsCourseQuery = [q|
        select  1
        from    Students
        where   addr = ?
    |]

createStudent :: DBM m => StudentId -> m StudentId
createStudent student = do
    execute createStudentRequest (Only student)
    return student
  where
    createStudentRequest = [q|
        insert into  Students
        values       (?)
    |]

createAssignment :: DBM m => Assignment -> m (Hash Assignment)
createAssignment assignment = do
    let courseId = assignment^.aCourseId

    courseExists <- existsCourse courseId

    unless courseExists $ do
        throwM (CourseDoesNotExist courseId)

    execute createAssignmentRequest
        ( assignmentHash
        , assignment^.aCourseId
        , assignment^.aType
        , assignment^.aAssignment
        )
    return assignmentHash
  where
    createAssignmentRequest = [q|
        insert into  Assignments
        values       (?,?,?,?)
    |]
    assignmentHash = hash assignment

getAssignment :: DBM m => Hash Assignment -> m (Maybe Assignment)
getAssignment assignmentHash = do
    listToMaybe <$> query getAssignmentQuery (Only assignmentHash)
  where
    getAssignmentQuery = [q|
        select  course_id, contents_hash, desc
        from    Assignments
        where   hash = ?
    |]

getSignedSubmission :: DBM m => PublicKey -> Hash Submission -> m (Maybe SignedSubmission)
getSignedSubmission pk submissionHash = do
    (listToMaybe . map ($ pk)) <$> query getSignedSubmissionQuery (Only submissionHash)
  where
    getSignedSubmissionQuery = [q|
        select     student_addr,
                   Submissions.contents_hash,
                   Assignments.course_id,
                   Assignments.contents_hash,
                   Assignments.desc,
                   Submissions.signature

        from       Submissions

        left join  Assignments
               on  Assignments.hash = assignment_hash

        where      Submissions.hash = ?
    |]

createTransaction :: DBM m => PublicKey -> PrivateTx -> m (PrivateTxId)
createTransaction pk trans = do
    transaction $ do
        let ptid    = hash trans
            subHash = trans^.ptSignedSubmission.to hash

        _ <- getSignedSubmission pk ()

        execute createTransactionRequest
            ( ptid
            , subHash
            , trans^.ptGrade
            , trans^.ptTime
            , -1
            )
  where
    createTransactionRequest :: Query
    createTransactionRequest = [q|

    |]

exists :: ToRow a => DBM m => Query -> a -> m Bool
exists theQuery args = (not . null :: [Only Int] -> Bool) <$> query theQuery args

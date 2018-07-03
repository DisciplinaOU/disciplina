{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.Instances where

import Codec.Serialise as Codec (deserialise, serialise)

import Control.Lens (to)

import Database.SQLite.Simple (Only (..), Query)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Database.SQLite.Simple.ToField (ToField (..))
import Database.SQLite.Simple.ToRow (ToRow (..))

import Text.InterpolatedString.Perl6 (q)

import Dscp.Core.Serialise ()
import Dscp.Core.Types (Address (..), Assignment (..), AssignmentType, CourseId (..), Grade (..),
                        SignedSubmission (..), StudentId, SubjectId, Submission (..),
                        SubmissionType, SubmissionWitness (..), aAssignment, aCourseId, aType,
                        sAssignment, sStudentId, sType, ssSubmission, ssWitness, swSig)
import Dscp.Crypto (Hash, PublicKey, Signature, hash)
import Dscp.DB.SQLite.Class
import Dscp.Educator.Txs (PrivateTx (..))

data DomainError
    = CourseDoesNotExist      CourseId
    | StudentDoesNotExist     StudentId
    | AssignmentDoesNotExist (Hash Assignment)
    deriving (Show, Typeable)

instance Exception DomainError

type DBM m = (MonadSQLiteDB m, MonadCatch m)

instance FromField (Hash a)       where fromField f = Codec.deserialise <$> fromField f
instance FromField (Signature a)  where fromField f = Codec.deserialise <$> fromField f

-- TODO(kir): use #define to generate macros
instance FromField Address        where fromField f = Codec.deserialise <$> fromField f
instance FromField PublicKey      where fromField f = Codec.deserialise <$> fromField f
instance FromField SubjectId      where fromField f = Codec.deserialise <$> fromField f
instance FromField CourseId       where fromField f = CourseId          <$> fromField f
instance FromField Grade          where fromField f = Codec.deserialise <$> fromField f
instance FromField AssignmentType where fromField f = Codec.deserialise <$> fromField f
instance FromField SubmissionType where fromField f = Codec.deserialise <$> fromField f

instance ToField   (Hash a)       where toField     = toField . Codec.serialise
instance ToField   (Signature a)  where toField     = toField . Codec.serialise

instance ToField   Address        where toField     = toField . Codec.serialise
instance ToField   CourseId       where toField     = toField . getCourseId
instance ToField   AssignmentType where toField     = toField . Codec.serialise
instance ToField   SubmissionType where toField     = toField . Codec.serialise

instance FromRow   CourseId       where fromRow     = field
instance FromRow   Grade          where fromRow     = field

instance FromRow Assignment where
    fromRow = Assignment <$> field <*> field <*> field

instance ToRow Assignment where
    toRow task@ (Assignment course ty text) =
        [toField (hash task), toField course, toField ty, toField text]

instance FromRow Submission where
    fromRow = Submission <$> field <*> field <*> fromRow

-- These three structures depend on the public key, which is not in db.
-- Therefore, we have to plug it in somehow.
instance FromRow (PublicKey -> SubmissionWitness) where
    fromRow = (\sig pk -> SubmissionWitness pk sig) <$> field

instance FromRow (PublicKey -> SignedSubmission) where
    fromRow = (\sub wit pk -> SignedSubmission sub (wit pk)) <$> fromRow <*> fromRow

instance FromRow (PublicKey -> PrivateTx) where
    fromRow = (\sigSub grade time pk -> PrivateTx (sigSub pk) grade time) <$> fromRow <*> field <*> field

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

    _ <- existsStudent student        `assert`     StudentDoesNotExist    student
    _ <- existsCourse  course         `assert`     CourseDoesNotExist     course
    _ <- getAssignment assignmentHash `assertJust` AssignmentDoesNotExist assignmentHash

    execute setAssignmentAuthorRequest (student, assignmentHash)
    execute generateSubmissionRequest
        ( submissionHash
        , student
        , assignmentHash
        , submissionType
        , submissionSig
        )

    return submissionHash
  where
    setAssignmentAuthorRequest :: Query
    setAssignmentAuthorRequest = [q|
        insert into  StudentAssignments
        values      (?, ?)
    |]

    generateSubmissionRequest :: Query
    generateSubmissionRequest = [q|
        insert into  Submissions
        values       (?, ?, ?, ?, ?)
    |]

getGradesForCourseAssignments :: DBM m => StudentId -> CourseId -> m [Grade]
getGradesForCourseAssignments student course = do
    query getGradesForCourseAssignmentsQuery (course, student)
  where
    getGradesForCourseAssignmentsQuery :: Query
    getGradesForCourseAssignmentsQuery = [q|
        select     grade
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
        select     *
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
        throwM message

assertJust :: DBM m => m (Maybe a) -> DomainError -> m a
assertJust action message = do
    mb <- action

    whenNothing mb $ do
        throwM message

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


exists :: ToRow a => DBM m => Query -> a -> m Bool
exists theQuery args = (not . null :: [Only Int] -> Bool) <$> query theQuery args
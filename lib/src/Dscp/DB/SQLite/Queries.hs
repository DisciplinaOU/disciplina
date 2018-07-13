{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.Queries where

import Control.Lens (makePrisms)

import Data.Coerce (coerce)
import Data.List (groupBy)
import qualified Data.Set as Set (fromList)
import Data.Time.Clock (UTCTime, getCurrentTime)

import Database.SQLite.Simple (Only (..), Query)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.ToRow (ToRow (..))

import Text.InterpolatedString.Perl6 (q)

import Dscp.Core.Serialise ()
import Dscp.Core.Types (Assignment (..), Course, SignedSubmission (..), Student, Subject,
                        Submission (..), aContentsHash, aCourseId, aDesc, aType, sAssignment,
                        sContentsHash, sStudentId, ssSubmission, ssWitness)
import Dscp.Crypto (MerkleProof, getMerkleRoot, mkMerkleProof)
import qualified Dscp.Crypto.MerkleTree as MerkleTree (fromList)
import Dscp.DB.SQLite.BlockData (BlockData (..), TxInBlock (..), TxWithIdx (..))
import Dscp.DB.SQLite.Class (MonadSQLiteDB (..), transaction)
import Dscp.DB.SQLite.Instances ()
import Dscp.Crypto (MerkleProof, mkMerkleProof)
import Dscp.DB.SQLite.BlockData (BlockData (..), TxInBlock (..), TxWithIdx (..))
import Dscp.DB.SQLite.Class (MonadSQLiteDB (..))
import Dscp.DB.SQLite.Instances ()
import Dscp.DB.SQLite.Types (TxBlockIdx (TxInMempool))
import Dscp.Educator.Block (PrivateBlock (..), pbBody, pbHeader, pbbTxs, pbhAtgDelta, pbhBodyProof)
import Dscp.Educator.Txs (PrivateTx (..), ptGrade, ptSignedSubmission, ptTime)
import Dscp.Util (HasId (..), assert, assertJust, idOf)

data DomainError
    = CourseDoesNotExist
        { deCourseId :: Id Course }

    | StudentDoesNotExist
        { deStudentId :: Id Student }

    | AssignmentDoesNotExist
        { deAssignmentId :: Id Assignment }

    | StudentWasNotEnrolledOnTheCourse
        { deStudentId :: Id Student
        , deCourseId  :: Id Course }

    | StudentWasNotSubscribedOnAssignment
        { deStudentId    :: Id Student
        , deAssignmentId :: Id Assignment }

    | SubmissionDoesNotExist
        { deSubmissionId :: Id Submission }

    | TransactionDoesNotExist
        { deTransactionId :: Id PrivateTx }

    | BlockDoesNotExist
        { deBlockId :: Id PrivateBlock }

    | BlockProofIsCorrupted
        { deBlockId :: Id PrivateBlock }

    deriving (Show, Typeable, Eq)

-- Using records ^ to get sensible autoderived json instances.

makePrisms ''DomainError

instance Exception DomainError

type DBM m = MonadSQLiteDB m

-- | How can a student get a list of courses?
getStudentCourses :: DBM m => Id Student -> m [Id Course]
getStudentCourses student =
    query getStudentCoursesQuery (Only student)
  where
    getStudentCoursesQuery :: Query
    getStudentCoursesQuery = [q|
        select  course_id
        from    StudentCourses
        where   student_addr = ?
    |]

-- | How can a student enroll to a course?
enrollStudentToCourse :: DBM m => Id Student -> Id Course -> m ()
enrollStudentToCourse student course = do
    transaction $ do
        _ <- existsCourse  course  `assert` CourseDoesNotExist  course
        _ <- existsStudent student `assert` StudentDoesNotExist student

        execute enrollStudentToCourseRequest (student, course)
  where
    enrollStudentToCourseRequest :: Query
    enrollStudentToCourseRequest = [q|
        insert into  StudentCourses
        values       (?, ?)
    |]

-- | How can a student get a list of his current course assignments?
getStudentAssignments :: DBM m => Id Student -> Id Course -> m [Assignment]
getStudentAssignments student course = do
    query getStudentAssignmentsQuery (student, course)
  where
    getStudentAssignmentsQuery :: Query
    getStudentAssignmentsQuery = [q|
        select     course_id, contents_hash, type, desc
        from       StudentAssignments
        left join  Assignments
               on  assignment_hash = Assignments.hash
        where      student_addr    = ?
               and course_id       = ?
    |]

-- | How can a student submit a submission for assignment?
submitAssignment :: DBM m => SignedSubmission -> m (Id SignedSubmission)
submitAssignment = createSignedSubmission

-- How can a student see his grades for course assignments?
getGradesForCourseAssignments :: DBM m => Id Student -> Id Course -> m [PrivateTx]
getGradesForCourseAssignments student course = do
    query getGradesForCourseAssignmentsQuery (student, course)
  where
    getGradesForCourseAssignmentsQuery :: Query
    getGradesForCourseAssignmentsQuery = [q|
        -- getGradesForCourseAssignments

        select     Submissions.student_addr,
                   Submissions.contents_hash,
                   Assignments.course_id,

                   Assignments.contents_hash,
                   Assignments.type,
                   Assignments.desc,

                   Submissions.signature,
                   grade,
                   time

        from       Transactions

        left join  Submissions
               on  submission_hash = Submissions.hash

        left join  Assignments
               on  assignment_hash = Assignments.hash

        where      student_addr = ?
              and  Assignments.course_id = ?
    |]

-- | How can a student receive transactions with Merkle proofs which contain info about his grades and assignments?
getStudentTransactions :: DBM m => Id Student -> m [PrivateTx]
getStudentTransactions student = do
    query getStudentTransactionsQuery (Only student)
  where
    getStudentTransactionsQuery :: Query
    getStudentTransactionsQuery = [q|
        -- getStudentTransactions

        select     Submissions.student_addr,
                   Submissions.contents_hash,
                   Assignments.course_id,
                   Assignments.contents_hash,
                   Assignments.type,
                   Assignments.desc,
                   Submissions.signature,
                   grade,
                   time

        from       Transactions

        left join  Submissions
               on  submission_hash = Submissions.hash

        left join  Assignments
               on  assignment_hash = Assignments.hash

        where      student_addr = ?
    |]

getProvenStudentTransactionsSince :: DBM m => Id Student -> UTCTime -> m [(MerkleProof PrivateTx, [PrivateTx])]
getProvenStudentTransactionsSince studentId sinceTime = do
    transaction $ do
        -- Contains `(tx, idx, blockId)` map.
        txsBlockList <- getTxsBlockMap

        -- Bake `blockId -> [(tx, idx)]` map.
        let txsBlockMap = groupToAssocWith (_tibBlockId, _tibTx) txsBlockList

        forM txsBlockMap $ \(blockId, transactions) -> do
            blockData <- getBlockData blockId

            let indices = _twiTxIdx <$> transactions & Set.fromList
            let bodies  = _twiTx    <$> transactions
            let tree    = _bdTree blockData
            let pruned  = mkMerkleProof tree indices

            return (pruned, bodies)

  where
    groupToAssocWith :: Eq k => Ord k => (a -> k, a -> v) -> [a] -> [(k, [v])]
    groupToAssocWith (key, value)
        -- The `groupBy` should return `[NonEmpty a]`.
        = map      (\every@ (selected : _) -> (key selected, map value every))
        . groupBy  ((==) `on` key)
        . sortWith  key

    -- Returns, effectively, `[((PrivateTx, Idx), Id PrivateBlock)]`.
    getTxsBlockMap :: DBM m => m [TxInBlock]
    getTxsBlockMap = query getTxsBlockMapQuery (studentId, sinceTime)
      where
        getTxsBlockMapQuery = [q|
            -- getTxsBlockMapQuery

            select     Submissions.student_addr,
                       Submissions.contents_hash,
                       Assignments.course_id,
                       Assignments.contents_hash,
                       Assignments.type,
                       Assignments.desc,
                       Submissions.signature,
                       Transactions.grade,
                       Transactions.time,
                       Transactions.idx
                       BlockTxs.blk_idx

            from       BlockTxs
            left join  Transactions
                   on  tx_hash = Transactions.hash

            left join  Submissions
                   on  Transactions.submission_hash = Submissions.hash

            left join  Assignments
                   on  Submissions .assignment_hash = Assignments.hash

            where      student_addr  = ?
                  and  time         >= ?
                  and  idx          <> -1
        |]

    -- Returns `PrivateBlock` in normalized format, with metadata.
    getBlockData :: DBM m => Id PrivateBlock -> m BlockData
    getBlockData blockId = do
        (listToMaybe <$> query getBlockDataQuery (Only blockId))
            `assertJust` BlockDoesNotExist blockId
      where
        -- TODO: prune fields that aren't needed.
        getBlockDataQuery = [q|
            select  idx
                    hash
                    time
                    prev_hash
                    atg_delta
                    mroot
                    mtree
            from    Blocks
            where   idx = ?
        |]

createBlock :: DBM m => PrivateBlock -> m (Id PrivateBlock)
createBlock block = do
    transaction $ do
        let tree = MerkleTree.fromList txs
            root = getMerkleRoot tree

            txs  = block^.pbBody.pbbTxs
            txs' = zip [1..] (getId <$> txs)
            bid  = block^.idOf

        return (root == block^.pbHeader.pbhBodyProof) `assert` BlockProofIsCorrupted bid

        time <- liftIO getCurrentTime

        _    <- execute createBlockRequest
            ( bid
            , time
            , block^.pbHeader.pbhAtgDelta
            , root
            , tree
            )

        for_ txs' $ \(idx, tid) -> do
            _ <- getTransaction tid `assertJust` TransactionDoesNotExist tid

            execute setTxIndexRequest    (idx :: Word32, tid)
            execute assignToBlockRequest (bid, tid)

        return bid
  where
    createBlockRequest = [q|
        insert into Blocks
        from
        select      idx + 1, ?, ?, hash, ?, ?, ?
        from        Blocks
        limit       1
    |]

    setTxIndexRequest = [q|
        update  Transactions
        set     idx  = ?
        where   hash = ?
    |]

    assignToBlockRequest = [q|
        insert into BlockTxs
        values      (?, ?)
    |]

createSignedSubmission :: DBM m => SignedSubmission -> m (Id SignedSubmission)
createSignedSubmission sigSub = do
    let
        submission     = sigSub^.ssSubmission
        submissionSig  = sigSub^.ssWitness

        student        = submission^.sStudentId
        submissionHash = submission^.idOf
        submissionCont = submission^.sContentsHash
        assignment     = submission^.sAssignment

        assignmentHash = assignment^.idOf

    transaction $ do
        _ <- existsStudent student        `assert`     StudentDoesNotExist    student
        _ <- getAssignment assignmentHash `assertJust` AssignmentDoesNotExist assignmentHash
        _ <- isAssignedToStudent student assignmentHash
            `assert` StudentWasNotSubscribedOnAssignment student assignmentHash

        execute generateSubmissionRequest
            ( submissionHash
            , student
            , assignmentHash
            , submissionCont
            , submissionSig
            )

    return submissionHash
  where
    generateSubmissionRequest :: Query
    generateSubmissionRequest = [q|
        insert into  Submissions
        values       (?, ?, ?, ?, ?, null)
    |]

setStudentAssignment :: DBM m => Id Student -> Id Assignment -> m ()
setStudentAssignment studentId assignmentId = do
    transaction $ do
      _          <- existsStudent studentId    `assert`     StudentDoesNotExist    studentId
      assignment <- getAssignment assignmentId `assertJust` AssignmentDoesNotExist assignmentId

      let courseId = assignment^.aCourseId

      _ <- existsCourse            courseId `assert` CourseDoesNotExist                         courseId
      _ <- isEnrolledTo  studentId courseId `assert` StudentWasNotEnrolledOnTheCourse studentId courseId

      execute setStudentAssignmentRequest (studentId, assignmentId)
  where
    setStudentAssignmentRequest :: Query
    setStudentAssignmentRequest = [q|
        insert into  StudentAssignments
        values      (?, ?)
    |]

isEnrolledTo :: DBM m => Id Student -> Id Course -> m Bool
isEnrolledTo studentId courseId = do
    exists enrollmentQuery (studentId, courseId)
  where
    enrollmentQuery = [q|
        select  count(*)
        from    StudentCourses
        where   student_addr = ?
           and  course_id    = ?
    |]

isAssignedToStudent :: DBM m => Id Student -> Id Assignment -> m Bool
isAssignedToStudent student assignment = do
    exists getStudentAssignmentQuery (student, assignment)
  where
    getStudentAssignmentQuery :: Query
    getStudentAssignmentQuery = [q|
        select  count(*)
        from    StudentAssignments
        where   student_addr = ?
           and  assignment_hash = ?
    |]


createCourse :: DBM m => Course -> Maybe Text -> [Id Subject] -> m (Id Course)
createCourse course desc subjects = do
    transaction $ do
        execute createCourseRequest (course, desc)
        for_ subjects $ \subject -> do
            execute attachSubjectToCourseRequest (subject, course)
        return course
  where
    createCourseRequest = [q|
        insert into  Courses
        values       (?, ?)
    |]

    attachSubjectToCourseRequest = [q|
        insert into  Subjects
        values       (?, ?, "")
    |]

getCourseSubjects :: DBM m => Course -> m [Subject]
getCourseSubjects course = do
    subjects :: [Only Subject] <- query getCourceSubjectsQuery (Only course)
    return (coerce subjects)
  where
    getCourceSubjectsQuery = [q|
        select id
        from   Subjects
        where  course_id = ?
    |]

existsCourse :: DBM m => Id Course -> m Bool
existsCourse course = do
    exists existsCourseQuery (Only course)
  where
    existsCourseQuery = [q|
        select  count(*)
        from    Courses
        where   id = ?
    |]

existsStudent :: DBM m => Id Student -> m Bool
existsStudent student = do
    exists existsCourseQuery (Only student)
  where
    existsCourseQuery = [q|
        select  count(*)
        from    Students
        where   addr = ?
    |]

createStudent :: DBM m => Student -> m (Id Student)
createStudent student = do
    execute createStudentRequest (Only student)
    return student
  where
    createStudentRequest = [q|
        insert into  Students
        values       (?)
    |]

createAssignment :: DBM m => Assignment -> m (Id Assignment)
createAssignment assignment = do
    let courseId = assignment^.aCourseId

    _ <- existsCourse courseId `assert` CourseDoesNotExist courseId

    execute createAssignmentRequest
        ( assignmentHash
        , assignment^.aCourseId
        , assignment^.aContentsHash
        , assignment^.aType
        , assignment^.aDesc
        )
    return assignmentHash
  where
    createAssignmentRequest = [q|
        insert into  Assignments
        values       (?,?,?,?,?)
    |]
    assignmentHash = assignment^.idOf

getAssignment :: DBM m => Id Assignment -> m (Maybe Assignment)
getAssignment assignmentHash = do
    listToMaybe <$> query getAssignmentQuery (Only assignmentHash)
  where
    getAssignmentQuery = [q|
        select  course_id, contents_hash, type, desc
        from    Assignments
        where   hash = ?
    |]

getSignedSubmission :: DBM m => Id SignedSubmission -> m (Maybe SignedSubmission)
getSignedSubmission submissionHash = do
    listToMaybe <$> query getSignedSubmissionQuery (Only submissionHash)
  where
    getSignedSubmissionQuery = [q|
        -- from 'getSignedSubmission'
        select     student_addr,
                   Submissions.contents_hash,
                   Assignments.course_id,
                   Assignments.contents_hash,
                   Assignments.type,
                   Assignments.desc,
                   Submissions.signature

        from       Submissions

        left join  Assignments
               on  Assignments.hash = assignment_hash

        where      Submissions.hash = ?
    |]

createTransaction :: (DBM m, ToField TxBlockIdx) => PrivateTx -> m (Id PrivateTx)
createTransaction trans = do
    transaction $ do
        let ptid    = trans^.idOf
            subHash = trans^.ptSignedSubmission.idOf

        _ <- getSignedSubmission subHash `assertJust`
            SubmissionDoesNotExist subHash

        execute createTransactionRequest
            ( ptid
            , subHash
            , trans^.ptGrade
            , trans^.ptTime
            , TxInMempool
            )

        return ptid
  where
    createTransactionRequest :: Query
    createTransactionRequest = [q|
        insert into  Transactions
        values       (?, ?, ?, ?, ?)
    |]

getTransaction :: DBM m => Id PrivateTx -> m (Maybe PrivateTx)
getTransaction ptid = do
    listToMaybe <$> query getTransactionQuery (Only ptid)
  where
    getTransactionQuery = [q|
        -- from 'getTransaction'

        select     Submissions.student_addr,
                   Submissions.contents_hash,
                   Assignments.course_id,
                   Assignments.contents_hash,
                   Assignments.type,
                   Assignments.desc,
                   Submissions.signature,
                   grade,
                   time

        from       Transactions

        left join  Submissions
               on  submission_hash = Submissions.hash

        left join  Assignments
               on  assignment_hash = Assignments.hash

        where      Transactions.hash = ?
    |]

exists :: ToRow a => DBM m => Query -> a -> m Bool
exists theQuery args = ((/= [[0]]) :: [[Int]] -> Bool) <$> query theQuery args

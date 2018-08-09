{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.Queries
       ( -- * Domain-level database errors (missing entites, mostly)
         DomainError(..)

         -- * Prisms for constructors
       , _CourseDoesNotExist
       , _StudentDoesNotExist
       , _AssignmentDoesNotExist
       , _StudentWasNotEnrolledOnTheCourse
       , _StudentWasNotSubscribedOnAssignment
       , _SubmissionDoesNotExist
       , _TransactionDoesNotExist
       , _BlockWithIndexDoesNotExist
         -- * Synonym for MonadSQLiteDB
       , DBM

         -- * Readonly actions
       , getCourseSubjects
       , getStudentCourses
       , getStudentAssignments
       , getGradesForCourseAssignments
       , getStudentTransactions
       , getProvenStudentTransactionsSince
       , getAssignment
       , getSignedSubmission
       , getTransaction

         -- * Readonly predicates
       , isEnrolledTo
       , isAssignedToStudent
       , existsCourse
       , existsStudent

         -- * Destructive actions
       , enrollStudentToCourse
       , submitAssignment
       , createBlock
       , createSignedSubmission
       , setStudentAssignment
       , createCourse
       , createStudent
       , createAssignment
       , createTransaction
       ) where


import Control.Lens (makePrisms)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Coerce (coerce)
import qualified Data.Map as Map (empty, fromList, insertWith, toList)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Only (..), Query)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.ToRow (ToRow (..))
import Text.InterpolatedString.Perl6 (q)

import Dscp.Core
import Dscp.Crypto (Hash, MerkleProof, fillEmptyMerkleTree, getEmptyMerkleTree, getMerkleRoot, hash)
import qualified Dscp.Crypto.MerkleTree as MerkleTree (fromList)
import Dscp.DB.SQLite.BlockData (BlockData (..), TxInBlock (..), TxWithIdx (..))
import Dscp.DB.SQLite.Class (MonadSQLiteDB (..), WithinSQLTransaction, transaction)
import Dscp.DB.SQLite.Instances ()
import Dscp.DB.SQLite.Types (TxBlockIdx (TxInMempool))
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

    | BlockWithIndexDoesNotExist
        { deBlockIdx :: Word32 }

    deriving (Show, Typeable, Eq)

-- Using records ^ to get sensible autoderived json instances.

makePrisms ''DomainError

deriveJSON defaultOptions ''DomainError
instance Exception DomainError

type DBM m = (MonadSQLiteDB m, MonadThrow m)

-- | How can a student get a list of courses?
getStudentCourses :: DBM m => Id Student -> m [Id Course]
getStudentCourses student =
    query getStudentCoursesQuery (Only student)
  where
    getStudentCoursesQuery :: Query
    getStudentCoursesQuery = [q|
        -- getStudentCourses
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
        -- enrollStudentToCourse
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
        -- getStudentAssignments
        select     course_id, contents_hash, type, desc
        from       StudentAssignments
        left join  Assignments
               on  assignment_hash = Assignments.hash
        where      student_addr    = ?
               and course_id       = ?
    |]

-- | How can a student submit a submission for assignment?
submitAssignment
    :: (DBM m, WithinSQLTransaction)
    => SignedSubmission -> m (Id SignedSubmission)
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
                   Assignments.hash,

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
                   Assignments.hash,
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

-- | Returns list of transaction blocks along with block-proof of a student since given moment.
getProvenStudentTransactionsSince :: DBM m => Id Student -> UTCTime -> m [(MerkleProof PrivateTx, [(Word32, PrivateTx)])]
getProvenStudentTransactionsSince studentId sinceTime = do
    transaction $ do
        -- Contains `(tx, idx, blockId)` map.
        txsBlockList <- getTxsBlockMap

        -- Bake `blockId -> [(tx, idx)]` map.
        let txsBlockMap = groupToAssocWith (_tibBlockId, _tibTx) txsBlockList
                      -- TODO: remove if transaction order is not needed to be preserved
                      <&> (<&> reverse)

        results <- forM txsBlockMap $ \(blockId, transactions) -> do
            blockData <- getBlockData blockId

            let tree    = _bdTree blockData
                indiced = [(idx, tx) | TxWithIdx tx idx <- transactions]
                mapping = Map.fromList indiced
                pruned  = fillEmptyMerkleTree mapping tree

            return (pruned, indiced)

        return [(proof, txs) | (Just proof, txs) <- results]

  where
    groupToAssocWith :: Ord k => Ord k => (a -> k, a -> v) -> [a] -> [(k, [v])]
    groupToAssocWith (key, value) =
        Map.toList . foldr' push Map.empty
      where
        push a = Map.insertWith (flip (++)) (key a) [value a]

    -- Returns, effectively, `[(tx, idx, blockId)]`.
    getTxsBlockMap :: DBM m => m [TxInBlock]
    getTxsBlockMap = do
        query getTxsBlockMapQuery (studentId, sinceTime)
      where
        getTxsBlockMapQuery = [q|
            -- getTxsBlockMapQuery

            select     Submissions.student_addr,
                       Submissions.contents_hash,
                       Assignments.hash,
                       Submissions.signature,
                       Transactions.grade,
                       Transactions.time,
                       Transactions.idx,
                       BlockTxs.blk_idx

            from       BlockTxs
            left join  Transactions
                   on  tx_hash = Transactions.hash

            left join  Submissions
                   on  Transactions.submission_hash = Submissions.hash

            left join  StudentAssignments
                   on  StudentAssignments.assignment_hash = Submissions.assignment_hash

            left join  Assignments
                   on  StudentAssignments.assignment_hash = Assignments.hash

            where      StudentAssignments.student_addr  =  ?
                  and  time                            >=  ?
                  and  Transactions.idx                <> -1
        |]

    -- Returns `PrivateBlock` in normalized format, with metadata.
    getBlockData :: DBM m => Word32 -> m BlockData
    getBlockData blockIdx = do
        (listToMaybe <$> query getBlockDataQuery (Only blockIdx))
            `assertJust` BlockWithIndexDoesNotExist blockIdx
      where
        getBlockDataQuery = [q|
            -- getBlockData
            select  idx,
                    hash,
                    time,
                    prev_hash,
                    atg_delta,
                    mroot,
                    mtree
            from    Blocks
            where   idx = ?
        |]

getAllNonChainedTransactions :: DBM m => m [PrivateTx]
getAllNonChainedTransactions = do
    query getAllNonChainedTransactionsQuery ()
  where
    getAllNonChainedTransactionsQuery = [q|
        -- getAllNonChainedTransactions
        select     Submissions.student_addr,
                   Submissions.contents_hash,
                   Assignments.hash,
                   Submissions.signature,
                   grade,
                   time

        from       Transactions

        left join  Submissions
               on  submission_hash = Submissions.hash

        left join  Assignments
               on  assignment_hash = Assignments.hash

        where  idx = -1
    |]

getLastBlockIdAndIdx :: DBM m => m (Hash PrivateBlockHeader, Word32)
getLastBlockIdAndIdx = do
    fromMaybe (genesisHeaderHash, 1) . listToMaybe <$> query [q|
        -- getLastBlockIdAndIdx
        select    hash,
                  idx
        from      Blocks
        order by  idx desc
        limit     1
    |] ()

createBlock :: DBM m => Maybe ATGDelta -> m ()
createBlock delta = do
    (prev, idx) <- getLastBlockIdAndIdx
    txs         <- getAllNonChainedTransactions

    let tree = MerkleTree.fromList txs
        root = getMerkleRoot tree

        txs' = zip [0..] (getId <$> txs)
        bid  = idx + 1

        trueDelta = mempty `fromMaybe` delta

        hdr = PrivateBlockHeader prev root trueDelta

    time <- liftIO getCurrentTime

    transaction $ do
        _ <- execute createBlockRequest
            ( bid
            , hash hdr
            , time
            , prev
            , trueDelta
            , root
            , getEmptyMerkleTree tree
            )

        for_ txs' $ \(txIdx, txId) -> do
            execute setTxIndexRequest    (txIdx :: Word32, txId)
            execute assignToBlockRequest (bid, txId)

        return ()
  where
    createBlockRequest = [q|
        -- createBlock
        insert into Blocks
        values      (?, ?, ?, ?, ?, ?, ?)
    |]

    setTxIndexRequest = [q|
        -- setTxIndex
        update  Transactions
        set     idx  = ?
        where   hash = ?
    |]

    assignToBlockRequest = [q|
        -- assignToBlock
        insert into BlockTxs
        values      (?, ?)
    |]

createSignedSubmission
    :: (DBM m, WithinSQLTransaction)
    => SignedSubmission -> m (Id SignedSubmission)
createSignedSubmission sigSub = do
    let
        submission     = sigSub^.ssSubmission
        submissionSig  = sigSub^.ssWitness

        student        = submission^.sStudentId
        submissionHash = submission^.idOf
        submissionCont = submission^.sContentsHash
        assignmentId   = submission^.sAssignmentHash

    _ <- existsStudent student        `assert`     StudentDoesNotExist    student
    _ <- getAssignment assignmentId `assertJust` AssignmentDoesNotExist assignmentId
    _ <- isAssignedToStudent student assignmentId
        `assert` StudentWasNotSubscribedOnAssignment student assignmentId

    currentTime <- liftIO getCurrentTime

    execute generateSubmissionRequest
        ( submissionHash
        , student
        , assignmentId
        , submissionCont
        , submissionSig
        , currentTime
        )

    return submissionHash
  where
    generateSubmissionRequest :: Query
    generateSubmissionRequest = [q|
        -- generateSubmission
        insert into  Submissions
        values       (?, ?, ?, ?, ?, julianday(?))
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
        -- setStudentAssignment
        insert into  StudentAssignments
        values      (?, ?)
    |]

isEnrolledTo :: DBM m => Id Student -> Id Course -> m Bool
isEnrolledTo studentId courseId = do
    exists enrollmentQuery (studentId, courseId)
  where
    enrollmentQuery = [q|
        -- isEnrolledTo
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
        -- isAssignedToStudent
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
        -- createCourse
        insert into  Courses
        values       (?, ?)
    |]

    attachSubjectToCourseRequest = [q|
        -- attachSubjectToCourse
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
        -- existsCourse
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
        ( assignmentId
        , assignment^.aCourseId
        , assignment^.aContentsHash
        , assignment^.aType
        , assignment^.aDesc
        )
    return assignmentId
  where
    createAssignmentRequest = [q|
        insert into  Assignments
        values       (?,?,?,?,?)
    |]
    assignmentId = assignment^.idOf

getAssignment :: DBM m => Id Assignment -> m (Maybe Assignment)
getAssignment assignmentId = do
    listToMaybe <$> query getAssignmentQuery (Only assignmentId)
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
                   Assignments.hash,
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
                   Assignments.hash,
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

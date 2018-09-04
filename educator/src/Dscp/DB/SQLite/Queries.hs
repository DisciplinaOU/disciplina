{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.Queries
       ( -- * Domain-level database errors (missing entites, mostly)
         DomainError (..)
       , DomainErrorItem (..)
       , DatabaseSemanticError (..)

         -- * Prisms for constructors
       , _AbsentError
       , _AlreadyPresentError
       , _SemanticError
       , _CourseDomain
       , _StudentDomain
       , _AssignmentDomain
       , _StudentCourseEnrollmentDomain
       , _StudentAssignmentSubscriptionDomain
       , _SubmissionDomain
       , _TransactionDomain
       , _BlockWithIndexDomain
       , _DeletingGradedSubmission
       , _StudentIsActiveError
         -- * Synonym for MonadSQLiteDB
       , DBM

         -- * Utils
       , checkExists
       , ifAlreadyExistsThrow
       , onReferenceInvalidThrow

         -- * Readonly actions
       , GetProvenStudentTransactionsFilters (..)
       , getCourseSubjects
       , getStudentCourses
       , getStudentAssignments
       , getGradesForCourseAssignments
       , getStudentTransactions
       , getProvenStudentTransactions
       , getAssignment
       , getSignedSubmission
       , getTransaction

         -- * Readonly predicates
       , isEnrolledTo
       , isAssignedToStudent
       , existsCourse
       , existsStudent
       , existsSubmission

         -- * Destructive actions
       , CourseDetails (..)
       , simpleCourse
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


import Control.Exception.Safe (catchJust)
import Control.Lens (makePrisms)
import Data.Coerce (coerce)
import Data.Default (Default (..))
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
import Dscp.DB.SQLite.Error (asAlreadyExistsError, asReferenceInvalidError)
import Dscp.DB.SQLite.Instances ()
import Dscp.DB.SQLite.Types (TxBlockIdx (TxInMempool))
import Dscp.DB.SQLite.Util
import Dscp.Util (HasId (..), idOf)

data DomainError
    = AbsentError DomainErrorItem
    | AlreadyPresentError DomainErrorItem
    | SemanticError DatabaseSemanticError
    deriving (Show, Eq)

data DomainErrorItem
    = CourseDomain
        { deCourseId :: Id Course }

    | StudentDomain
        { deStudentId :: Id Student }

    | AssignmentDomain
        { deAssignmentId :: Id Assignment }

    | StudentCourseEnrollmentDomain
        { deStudentId :: Id Student
        , deCourseId  :: Id Course }

    | StudentAssignmentSubscriptionDomain
        { deStudentId    :: Id Student
        , deAssignmentId :: Id Assignment }

    | SubmissionDomain
        { deSubmissionId :: Id Submission }

    | TransactionDomain
        { deTransactionId :: Id PrivateTx }

    | BlockWithIndexDomain
        { deBlockIdx :: Word32 }

    deriving (Show, Typeable, Eq)

-- | Logical errors.
data DatabaseSemanticError
    = StudentIsActiveError     (Id Student)
      -- ^ Student can't be deleted because it has activities.
    | DeletingGradedSubmission (Id Submission)
      -- ^ Submission has potentially published grade and thus can't be deleted.
    deriving (Show, Eq)

makePrisms ''DomainError
makePrisms ''DomainErrorItem
makePrisms ''DatabaseSemanticError

instance Exception DomainError

-- | When query starts with @select(*)@, checks that non empty set of rows is
-- returned.
checkExists :: ToRow a => DBM m => Query -> a -> m Bool
checkExists theQuery args = ((/= [[0]]) :: [[Int]] -> Bool) <$> query theQuery args

-- | Catch "unique" constraint violation and rethrow specific error.
ifAlreadyExistsThrow :: MonadCatch m => m a -> DomainErrorItem -> m a
ifAlreadyExistsThrow action err =
    catchJust asAlreadyExistsError action
        (\_ -> throwM $ AlreadyPresentError err)

assertExists :: MonadCatch m => m Bool -> DomainErrorItem -> m ()
assertExists action = assertJustPresent (bool Nothing (Just ()) <$> action)

assertJustPresent :: MonadCatch m => m (Maybe a) -> DomainErrorItem -> m a
assertJustPresent action err =
    action >>= maybe (throwM $ AbsentError err) pure

onReferenceInvalidThrow :: (MonadCatch m, Exception e) => m a -> e -> m a
onReferenceInvalidThrow action err =
    catchJust asReferenceInvalidError action (\_ -> throwM err)

type DBM m = (MonadSQLiteDB m, MonadCatch m)

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
        existsCourse  course  `assertExists` CourseDomain  course
        existsStudent student `assertExists` StudentDomain student

        execute enrollStudentToCourseRequest (student, course)
            `ifAlreadyExistsThrow` StudentCourseEnrollmentDomain student course
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

data GetProvenStudentTransactionsFilters = GetProvenStudentTransactionsFilters
    { pfCourse :: Maybe Course
    , pfSince  :: Maybe UTCTime
    } deriving (Show, Generic)

instance Default GetProvenStudentTransactionsFilters where
    def = GetProvenStudentTransactionsFilters def def

-- | Returns list of transaction blocks along with block-proof of a student since given moment.
getProvenStudentTransactions
    :: DBM m
    => Id Student -> GetProvenStudentTransactionsFilters -> m [(MerkleProof PrivateTx, [(Word32, PrivateTx)])]
getProvenStudentTransactions studentId filters = do
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
        query getTxsBlockMapQuery (oneParam studentId <> mconcat filteringParams)
      where
        (filteringClauses, filteringParams) = unzip
            [ mkFilter "time >= ?"     $ pfSince filters
            , mkFilter "course_id = ?" $ pfCourse filters
            ]

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
                  and  Transactions.idx                <> -1
        |]
          `filterClauses` filteringClauses

    -- Returns `PrivateBlock` in normalized format, with metadata.
    getBlockData :: DBM m => Word32 -> m BlockData
    getBlockData blockIdx = do
        (listToMaybe <$> query getBlockDataQuery (Only blockIdx))
            `assertJustPresent` BlockWithIndexDomain blockIdx
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
            `ifAlreadyExistsThrow` BlockWithIndexDomain bid

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

    _ <- existsStudent student      `assertExists`     StudentDomain   student
    _ <- getAssignment assignmentId `assertJustPresent` AssignmentDomain assignmentId
    _ <- isAssignedToStudent student assignmentId
        `assertExists` StudentAssignmentSubscriptionDomain student assignmentId

    currentTime <- liftIO getCurrentTime

    execute generateSubmissionRequest
        ( submissionHash
        , student
        , assignmentId
        , submissionCont
        , submissionSig
        , currentTime
        )
        `ifAlreadyExistsThrow` SubmissionDomain submissionHash

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
      _          <- existsStudent studentId    `assertExists`     StudentDomain    studentId
      assignment <- getAssignment assignmentId `assertJustPresent` AssignmentDomain assignmentId

      let courseId = assignment^.aCourseId

      _ <- existsCourse            courseId `assertExists` CourseDomain                         courseId
      _ <- isEnrolledTo  studentId courseId `assertExists` StudentCourseEnrollmentDomain studentId courseId

      execute setStudentAssignmentRequest (studentId, assignmentId)
          `ifAlreadyExistsThrow` StudentAssignmentSubscriptionDomain studentId assignmentId
  where
    setStudentAssignmentRequest :: Query
    setStudentAssignmentRequest = [q|
        -- setStudentAssignment
        insert into  StudentAssignments
        values      (?, ?)
    |]

isEnrolledTo :: DBM m => Id Student -> Id Course -> m Bool
isEnrolledTo studentId courseId = do
    checkExists enrollmentQuery (studentId, courseId)
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
    checkExists getStudentAssignmentQuery (student, assignment)
  where
    getStudentAssignmentQuery :: Query
    getStudentAssignmentQuery = [q|
        -- isAssignedToStudent
        select  count(*)
        from    StudentAssignments
        where   student_addr = ?
           and  assignment_hash = ?
    |]

data CourseDetails = CourseDetails
    { cdCourseId :: Course
    , cdDesc     :: Text
    , cdSubjects :: [Id Subject]
    } deriving (Show, Generic)

simpleCourse :: Course -> CourseDetails
simpleCourse i = CourseDetails i "" []

createCourse :: DBM m => CourseDetails -> m (Id Course)
createCourse params = do
    let course = cdCourseId params
    transaction $ do
        execute createCourseRequest (course, cdDesc params)
            `ifAlreadyExistsThrow` CourseDomain course
        for_ (cdSubjects params) $ \subject -> do
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
    checkExists existsCourseQuery (Only course)
  where
    existsCourseQuery = [q|
        -- existsCourse
        select  count(*)
        from    Courses
        where   id = ?
    |]

existsStudent :: DBM m => Id Student -> m Bool
existsStudent student = do
    checkExists existsCourseQuery (Only student)
  where
    existsCourseQuery = [q|
        select  count(*)
        from    Students
        where   addr = ?
    |]

existsSubmission :: DBM m => Id Submission -> m Bool
existsSubmission submission = do
    checkExists existsSubmissionQuery (Only submission)
  where
    existsSubmissionQuery = [q|
        select  count(*)
        from    Submissions
        where   hash = ?
    |]

createStudent :: DBM m => Student -> m (Id Student)
createStudent student = do
    execute createStudentRequest (Only student)
        `ifAlreadyExistsThrow` StudentDomain student
    return student
  where
    createStudentRequest = [q|
        insert into  Students
        values       (?)
    |]

createAssignment :: DBM m => Assignment -> m (Id Assignment)
createAssignment assignment = do
    let courseId = assignment^.aCourseId

    _ <- existsCourse courseId `assertExists` CourseDomain courseId

    execute createAssignmentRequest
        ( assignmentId
        , assignment^.aCourseId
        , assignment^.aContentsHash
        , assignment^.aType
        , assignment^.aDesc
        )
        `ifAlreadyExistsThrow` AssignmentDomain assignmentId
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

        _ <- getSignedSubmission subHash `assertJustPresent`
            SubmissionDomain subHash

        execute createTransactionRequest
            ( ptid
            , subHash
            , trans^.ptGrade
            , trans^.ptTime
            , TxInMempool
            )
            `ifAlreadyExistsThrow` TransactionDomain ptid

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

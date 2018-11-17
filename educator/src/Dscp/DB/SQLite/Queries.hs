{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes    #-}

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
       , nullCourse
       , simpleCourse
       , enrollStudentToCourse
       , submitAssignment
       , getLastBlockIdAndIdx
       , getPrivateBlock
       , getPrivateBlockIdxByHash
       , getPrivateBlocksAfter
       , getPrivateBlocksAfterHash
       , createPrivateBlock
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
import Database.Beam.Query (aggregate_, all_, countAll_, desc_, exists_, filter_, guard_, insert,
                            insertValues, limit_, orderBy_, references_, related_, select, val_,
                            (==.))
import Database.Beam.Schema (pk)
import Database.SQLite.Simple (Only (..), Query)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.ToRow (ToRow (..))
import Snowdrop.Util (OldestFirst (..))
import Text.InterpolatedString.Perl6 (q)

import Dscp.Core
import Dscp.Crypto (Hash, MerkleProof, fillEmptyMerkleTree, getEmptyMerkleTree, getMerkleRoot, hash)
import qualified Dscp.Crypto.MerkleTree as MerkleTree (fromList)
import Dscp.DB.SQLite.BlockData (BlockData (..), TxInBlock (..), TxWithIdx (..))
import Dscp.DB.SQLite.Error (asAlreadyExistsError, asReferenceInvalidError)
import Dscp.DB.SQLite.Functions
import Dscp.DB.SQLite.Instances ()
import Dscp.DB.SQLite.Schema
import Dscp.DB.SQLite.Types (TxBlockIdx (TxInMempool))
import Dscp.DB.SQLite.Util
import Dscp.Util

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

es = educatorSchema

-- | Catch "unique" constraint violation and rethrow specific error.
ifAlreadyExistsThrow :: MonadCatch m => m a -> DomainErrorItem -> m a
ifAlreadyExistsThrow action err =
    catchJust asAlreadyExistsError action
        (\_ -> throwM $ AlreadyPresentError err)

rewrapAlreadyExists :: MonadCatch m => DomainErrorItem -> m a -> m a
rewrapAlreadyExists = flip ifAlreadyExistsThrow

assertExists :: MonadCatch m => m Bool -> DomainErrorItem -> m ()
assertExists action = assertJustPresent (bool Nothing (Just ()) <$> action)

assertJustPresent :: MonadCatch m => m (Maybe a) -> DomainErrorItem -> m a
assertJustPresent action err =
    action >>= maybe (throwM $ AbsentError err) pure

-- | Catch "foreign" constraint violation and rethrow specific error.
onReferenceInvalidThrow :: (MonadCatch m, Exception e) => m a -> e -> m a
onReferenceInvalidThrow action err =
    catchJust asReferenceInvalidError action (\_ -> throwM err)

type DBM m = (MonadIO m, MonadCatch m)

-- | How can a student get a list of courses?
getStudentCourses :: MonadIO m => Id Student -> DBT t w m [Id Course]
getStudentCourses student' =
    runSelect . select $ do
        student :-: course <- all_ (esStudentCourses es)
        guard_ (student ==. valPk_ student')
        return course

-- | How can a student enroll to a course?
enrollStudentToCourse :: DBM m => Id Student -> Id Course -> DBT t 'Writing m ()
enrollStudentToCourse student course = do
    -- TODO: ensure foreign constraints check will play for us
    runInsert . insert (esStudentCourses es) $
        insertValues [student <:-:> course]

-- | How can a student get a list of his current course assignments?
getStudentAssignments
    :: MonadIO m
    => Id Student -> Id Course -> DBT t w m [Assignment]
getStudentAssignments student' course' = do
    runSelectMap assignmentFromRow . select $ do
        student :-: assignmentId <- all_ (esStudentAssignments es)
        assignment <- related_ (esAssignments es) assignmentId
        guard_ (student ==. valPk_ student')
        guard_ (arCourse assignment ==. val_ (CourseRowId course'))
        return assignment

-- | How can a student submit a submission for assignment?
submitAssignment
    :: DBM m
    => SignedSubmission -> DBT 'WithinTx 'Writing m (Id SignedSubmission)
submitAssignment = createSignedSubmission

-- How can a student see his grades for course assignments?
getGradesForCourseAssignments
    :: MonadIO m
    => Id Student -> Id Course -> DBT t w m [PrivateTx]
getGradesForCourseAssignments student' course' = do
    runSelectMap (uncurry privateTxFromRow) . select $ do
        privateTx <- all_ (esTransactions es)
        submission <- related_ (esSubmissions es) (trSubmissionHash privateTx)
        assignment <- related_ (esAssignments es) (srAssignmentHash submission)
        guard_ (srStudent submission ==. valPk_ student')
        guard_ (arCourse assignment ==. valPk_ course')
        return (privateTx, submission)

-- | How can a student receive transactions with Merkle proofs which contain info about his grades and assignments?
getStudentTransactions :: MonadIO m => Id Student -> DBT t w m [PrivateTx]
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
    { pfCourse     :: Maybe Course
    , pfStudent    :: Maybe Student
    , pfAssignment :: Maybe (Hash Assignment)
    , pfSince      :: Maybe UTCTime
    } deriving (Show, Generic)

deriving instance Default GetProvenStudentTransactionsFilters

-- | Returns list of transaction blocks along with block-proof of a student since given moment.
getProvenStudentTransactions
    :: forall m w.
       DBM m
    => GetProvenStudentTransactionsFilters
    -> DBT 'WithinTx w m [(MerkleProof PrivateTx, [(Word32, PrivateTx)])]
getProvenStudentTransactions filters = do
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
    getTxsBlockMap :: DBT t w m [TxInBlock]
    getTxsBlockMap = do
        query getTxsBlockMapQuery $ mconcat filteringParams
      where
        (filteringClauses, filteringParams) = unzip
            [ mkFilter "time >= ?"     $ pfSince filters
            , mkFilter "course_id = ?" $ pfCourse filters
            , mkFilter "StudentAssignments.student_addr = ?" $ pfStudent filters
            , mkFilter "Assignments.hash = ?" $ pfAssignment filters
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

            where      Transactions.idx                <> -1
        |]
          `filterClauses` filteringClauses

    -- Returns `PrivateBlock` in normalized format, with metadata.
    getBlockData :: Word32 -> DBT t w m BlockData
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

getAllNonChainedTransactions :: MonadIO m => DBT t w m [PrivateTx]
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

genesisBlockIdx :: Word32
genesisBlockIdx = 1

getLastBlockIdAndIdx
    :: MonadIO m
    => DBT t w m (Hash PrivateBlockHeader, Word32)
getLastBlockIdAndIdx = do
    res <- runSelect . select $
        limit_ 1 $
        orderBy_ (desc_ . snd) $ do
            block <- all_ (esBlocks es)
            return (brHash block, brIdx block)
    if null res
       then return (genesisHeaderHash, genesisBlockIdx)
       else return (oneOrError res)

getPrivateBlock
    :: MonadIO m
    => Word32 -> DBT t w m (Maybe PrivateBlockHeader)
getPrivateBlock idx = do
    listToMaybe <$> query getPrivateBlockQuery (Only idx)
  where
    getPrivateBlockQuery = [q|
        -- getPrivateBlockQuery
        select    prev_hash, mroot, atg_delta
        from      Blocks
        where     idx = ?
    |]

getPrivateBlockIdxByHash
    :: MonadIO m
    => PrivateHeaderHash -> DBT t w m (Maybe Word32)
getPrivateBlockIdxByHash phHash
    | phHash == genesisHeaderHash = pure $ Just genesisBlockIdx
    | otherwise =
        fmap fromOnly . listToMaybe <$>
        query getPrivateBlockByHashQuery (Only phHash)
  where
    getPrivateBlockByHashQuery = [q|
        -- getPrivateBlockByHashQuery
        select    idx
        from      Blocks
        where     hash = ?
    |]

-- | Returns blocks starting from given one (including) up to the tip.
getPrivateBlocksAfter
    :: MonadIO m
    => Word32 -> DBT t w m (OldestFirst [] PrivateBlockHeader)
getPrivateBlocksAfter idx =
    OldestFirst <$> query getPrivateBlocksAfterQuery (Only idx)
  where
    getPrivateBlocksAfterQuery = [q|
        -- getPrivateBlockAfterQuery
        select    prev_hash, mroot, atg_delta
        from      Blocks
        where     idx > ?
        order by  idx asc
    |]

getPrivateBlocksAfterHash
    :: MonadIO m
    => PrivateHeaderHash -> DBT t w m (Maybe $ OldestFirst [] PrivateBlockHeader)
getPrivateBlocksAfterHash phHash = do
    midx <- getPrivateBlockIdxByHash phHash
    forM midx getPrivateBlocksAfter

createPrivateBlock
    :: DBM m
    => Maybe ATGDelta -> DBT 'WithinTx 'Writing m (Maybe PrivateBlockHeader)
createPrivateBlock delta = runMaybeT $ do
    (prev, idx) <- lift getLastBlockIdAndIdx
    txs         <- lift getAllNonChainedTransactions

    let tree = MerkleTree.fromList txs
        root = getMerkleRoot tree

        txs' = zip [0..] (getId <$> txs)
        bid  = idx + 1

        trueDelta = mempty `fromMaybe` delta

        hdr = PrivateBlockHeader prev root trueDelta

    let isNullBlock = and
            [ isEmptyATGDelta trueDelta
            , null txs
            ]
    guard (not isNullBlock)

    time <- liftIO getCurrentTime

    _ <- lift $ execute createBlockRequest
        ( bid
        , hash hdr
        , time
        , prev
        , trueDelta
        , root
        , getEmptyMerkleTree tree
        )
        `ifAlreadyExistsThrow` BlockWithIndexDomain bid

    for_ txs' $ \(txIdx, txId) -> lift $ do
        execute setTxIndexRequest    (txIdx :: Word32, txId)
        execute assignToBlockRequest (bid, txId)

    return hdr
  where
    createBlockRequest = [q|
        -- createPrivateBlock
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
    :: DBM m
    => SignedSubmission -> DBT 'WithinTx 'Writing m (Id SignedSubmission)
createSignedSubmission sigSub = do
    let
        submission     = sigSub^.ssSubmission
        submissionSig  = sigSub^.ssWitness

        student        = submission^.sStudentId
        submissionHash = submission^.idOf
        submissionCont = submission^.sContentsHash
        assignmentId   = submission^.sAssignmentHash

    _ <- existsStudent student      `assertExists`      StudentDomain   student
    _ <- getAssignment assignmentId `assertJustPresent` AssignmentDomain assignmentId
    _ <- isAssignedToStudent student assignmentId
        `assertExists` StudentAssignmentSubscriptionDomain student assignmentId

    currentTime <- liftIO getCurrentTime

    rewrapAlreadyExists (SubmissionDomain submissionHash) $
        runInsert . insert (esSubmissions es) . insertValue $
            SubmissionRow
            { srHash = submissionHash
            , srStudent = packPk $ submission^.sStudentId
            , srAssignmentHash = packPk $ submission^.sAssignmentHash
            , srContentsHash = submission^.sContentsHash
            , srSignature = sigSub^.ssWitness
            , srCreationTime = currentTime
            }

    return submissionHash
  where
    generateSubmissionRequest :: Query
    generateSubmissionRequest = [q|
        -- generateSubmission
        insert into  Submissions
        values       (?, ?, ?, ?, ?, julianday(?))
    |]

setStudentAssignment :: DBM m => Id Student -> Id Assignment -> DBT 'WithinTx 'Writing m ()
setStudentAssignment studentId assignmentId = do
    _          <- existsStudent studentId    `assertExists`      StudentDomain    studentId
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

isEnrolledTo :: MonadIO m => Id Student -> Id Course -> DBT t w m Bool
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

isAssignedToStudent
    :: MonadIO m
    => Id Student -> Id Assignment -> DBT t w m Bool
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
    { cdCourseId :: Maybe Course
    , cdDesc     :: Text
    , cdSubjects :: [Id Subject]
    } deriving (Show, Generic)

-- | Course without any specific information. For testing purposes.
nullCourse :: CourseDetails
nullCourse = CourseDetails{ cdCourseId = Nothing, cdDesc = "", cdSubjects = [] }

-- | Course with specific id but without any other information.
-- For testing purposes.
simpleCourse :: Course -> CourseDetails
simpleCourse i = nullCourse{ cdCourseId = Just i }

createCourse :: DBM m => CourseDetails -> DBT 'WithinTx 'Writing m (Id Course)
createCourse params = do
    course <- case cdCourseId params of
        Nothing -> do
            execute createCourseRequest (cdCourseId params, cdDesc params)
            Course . fromIntegral @Word64 @Word32 <$> sqlCall LastInsertRowId
        Just course -> do
            execute createCourseRequest (cdCourseId params, cdDesc params)
                `ifAlreadyExistsThrow` CourseDomain course
            return course
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

getCourseSubjects :: MonadIO m => Course -> DBT t w m [Subject]
getCourseSubjects course = do
    subjects :: [Only Subject] <- query getCourceSubjectsQuery (Only course)
    return (coerce subjects)
  where
    getCourceSubjectsQuery = [q|
        select id
        from   Subjects
        where  course_id = ?
    |]

existsCourse :: MonadIO m => Id Course -> DBT t w m Bool
existsCourse course' =
    checkExists $
        filter_ (\course -> pk course ==. valPk_ course')
                (all_ $ esCourses es)

existsStudent :: MonadIO m => Id Student -> DBT t w m Bool
existsStudent student = do
    checkExists existsCourseQuery (Only student)
  where
    existsCourseQuery = [q|
        select  count(*)
        from    Students
        where   addr = ?
    |]

existsSubmission :: MonadIO m => Id Submission -> DBT t w m Bool
existsSubmission submission = do
    checkExists existsSubmissionQuery (Only submission)
  where
    existsSubmissionQuery = [q|
        select  count(*)
        from    Submissions
        where   hash = ?
    |]

createStudent :: DBM m => Student -> DBT t 'Writing m (Id Student)
createStudent student = do
    execute createStudentRequest (Only student)
        `ifAlreadyExistsThrow` StudentDomain student
    return student
  where
    createStudentRequest = [q|
        insert into  Students
        values       (?)
    |]

createAssignment :: DBM m => Assignment -> DBT 'WithinTx 'Writing m (Id Assignment)
createAssignment assignment = do
    let courseId = assignment^.aCourseId

    _ <- existsCourse courseId `assertExists` CourseDomain courseId

    rewrapAlreadyExists (AssignmentDomain assignmentId) $
        runInsert . insert (esAssignments es) . insertValue $
            AssignmentRow
            { arHash = assignmentId
            , arCourse = packPk courseId
            , arContentsHash = assignment^.aContentsHash
            , arType = assignment^.aType
            , arDesc = assignment^.aDesc
            }

    return assignmentId
  where
    assignmentId = assignment^.idOf

getAssignment
    :: MonadIO m
    => Id Assignment -> DBT t w m (Maybe Assignment)
getAssignment assignmentId = do
    listToMaybe <$> query getAssignmentQuery (Only assignmentId)
  where
    getAssignmentQuery = [q|
        select  course_id, contents_hash, type, desc
        from    Assignments
        where   hash = ?
    |]

getSignedSubmission
    :: MonadIO m
    => Id SignedSubmission -> DBT t w m (Maybe SignedSubmission)
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

createTransaction
    :: (DBM m, ToField TxBlockIdx)
    => PrivateTx -> DBT 'WithinTx 'Writing m (Id PrivateTx)
createTransaction trans = do
    let ptid    = trans^.idOf
        subHash = trans^.ptSignedSubmission.idOf

    _ <- getSignedSubmission subHash `assertJustPresent`
        SubmissionDomain subHash

    rewrapAlreadyExists (TransactionDomain ptid) $
        runInsert . insert (esTransactions es) . insertValue $
            TransactionRow
            { trHash = ptid
            , trSubmissionHash = packPk subHash
            , trGrade = trans^.ptGrade
            , trCreationTime = trans^.ptTime
            , trIdx = TxInMempool
            }

    return ptid
  where
    createTransactionRequest :: Query
    createTransactionRequest = [q|
        insert into  Transactions
        values       (?, ?, ?, ?, ?)
    |]

getTransaction :: DBM m => Id PrivateTx -> DBT t w m (Maybe PrivateTx)
getTransaction ptid = do
    fmap listToMaybe . runSelectMap (uncurry privateTxFromRow) . select $ do
        privateTx <- all_ (esTransactions es)
        submission <- related_ (esSubmissions es) (trSubmissionHash privateTx)
        assignment <- related_ (esAssignments es) (srAssignmentHash submission)
        guard_ (pk privateTx ==. valPk_ ptid)
        return (privateTx, submission)

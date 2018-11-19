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
       , rewrapAlreadyExists

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
import Control.Lens (makePrisms, to)
import Data.Default (Default (..))
import qualified Data.Map as Map (empty, fromList, insertWith, toList)
import Data.Time.Clock (UTCTime)
import Snowdrop.Util (OldestFirst (..))

import Dscp.Core
import Dscp.Crypto (EmptyMerkleTree, Hash, MerkleProof, fillEmptyMerkleTree, getEmptyMerkleTree,
                    getMerkleRoot, hash)
import qualified Dscp.Crypto.MerkleTree as MerkleTree (fromList)
import Dscp.DB.SQLite.BlockData
import Dscp.DB.SQLite.Error (asAlreadyExistsError, asReferenceInvalidError)
import Dscp.DB.SQLite.Functions
import Dscp.DB.SQLite.Instances ()
import Dscp.DB.SQLite.Schema
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
        { deBlockIdx :: BlockIdx }

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

es :: DatabaseSettings be EducatorSchema
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
        student :-: CourseRowId course <- all_ (esStudentCourses es)
        guard_ (student ==. valPk_ student')
        return course

-- | How can a student enroll to a course?
enrollStudentToCourse :: DBM m => Id Student -> Id Course -> DBT 'WithinTx 'Writing m ()
enrollStudentToCourse student course = do
    -- TODO: try foreign constraints check
    existsCourse  course  `assertExists` CourseDomain  course
    existsStudent student `assertExists` StudentDomain student

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
    runSelectMap privateTxFromRow . select $ do
        privateTx <- all_ (esTransactions es)
        submission <- related_ (esSubmissions es) (trSubmissionHash privateTx)
        assignment <- related_ (esAssignments es) (srAssignmentHash submission)
        guard_ (srStudent submission ==. valPk_ student')
        guard_ (arCourse assignment ==. valPk_ course')
        return (privateTx, submission)

-- | How can a student receive transactions with Merkle proofs which contain info about his grades and assignments?
getStudentTransactions :: MonadIO m => Id Student -> DBT t w m [PrivateTx]
getStudentTransactions student' = do
    runSelectMap privateTxFromRow . select $ do
        privateTx <- all_ (esTransactions es)
        submission <- related_ (esSubmissions es) (trSubmissionHash privateTx)
        guard_ (srStudent submission ==. valPk_ student')
        return (privateTx, submission)

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
    -> DBT 'WithinTx w m [(MerkleProof PrivateTx, [(TxWithinBlockIdx, PrivateTx)])]
getProvenStudentTransactions filters = do
    -- Contains `(tx, idx, blockId)` map.
    txsBlockList <- getTxsBlockMap

    -- Bake `blockId -> [(tx, idx)]` map.
    let txsBlockMap = groupToAssocWith txsBlockList
                  -- TODO: remove if transaction order is not needed to be preserved
                  <&> (<&> reverse)

    results <- forM txsBlockMap $ \(blockId, transactions) -> do
        tree <- getMerkleTree blockId

        let mapping = Map.fromList $ map (first unTxWithinBlockIdx) transactions
            pruned  = fillEmptyMerkleTree mapping tree

        return (pruned, transactions)

    return [(proof, txs) | (Just proof, txs) <- results]
  where
    groupToAssocWith :: Ord k => [(k, v)] -> [(k, [v])]
    groupToAssocWith =
        Map.toList . foldr' push Map.empty
      where
        push a = Map.insertWith (flip (++)) (fst a) [snd a]

    getTxsBlockMap :: DBT t w m [(BlockIdx, (TxWithinBlockIdx, PrivateTx))]
    getTxsBlockMap =
        runSelectMap rearrange . select $ do
            txId :-: BlockRowId blockIdx <- all_ (esBlockTxs es)
            privateTx <- related_ (esTransactions es) txId
            submission <- related_ (esSubmissions es) (trSubmissionHash privateTx)

            guard_ (trIdx privateTx /=. val_ TxInMempool)

            whenJust (pfSince filters) $ \since ->
                guard_ (trCreationTime privateTx >=. val_ since)

            whenJust (pfCourse filters) $ \course -> do
                assignment <- related_ (esAssignments es) (srAssignmentHash submission)
                guard_ (arCourse assignment ==. valPk_ course)

            whenJust (pfStudent filters) $ \student ->
                guard_ (srStudent submission ==. valPk_ student)

            whenJust (pfAssignment filters) $ \assignmentHash ->
                guard_ (srAssignmentHash submission ==. valPk_ assignmentHash)

            return (blockIdx, (privateTx, submission))
      where
        rearrange (bi, (tx, sub)) =
            let TxBlockIdx txIdx = trIdx tx
            in (bi, (txIdx, privateTxFromRow (tx, sub)))

    getMerkleTree :: BlockIdx -> DBT t w m (EmptyMerkleTree PrivateTx)
    getMerkleTree blockIdx =
        nothingToThrow (AbsentError $ BlockWithIndexDomain blockIdx) =<<
        selectByPk brMerkleTree (esBlocks es) blockIdx

getAllNonChainedTransactions :: MonadIO m => DBT t w m [PrivateTx]
getAllNonChainedTransactions =
    runSelectMap privateTxFromRow . select $ do
        privateTx <- all_ (esTransactions es)
        submission <- related_ (esSubmissions es) (trSubmissionHash privateTx)
        guard_ (trIdx privateTx ==. val_ TxInMempool)
        return (privateTx, submission)

genesisBlockIdx :: BlockIdx
genesisBlockIdx = 1

getLastBlockIdAndIdx
    :: MonadIO m
    => DBT t w m (Hash PrivateBlockHeader, BlockIdx)
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
    => BlockIdx -> DBT t w m (Maybe PrivateBlockHeader)
getPrivateBlock = selectByPk pbHeaderFromRow (esBlocks es)

-- TODO: requires index on Blocks.hash
getPrivateBlockIdxByHash
    :: MonadIO m
    => PrivateHeaderHash -> DBT t w m (Maybe BlockIdx)
getPrivateBlockIdxByHash phHash
    | phHash == genesisHeaderHash = pure $ Just genesisBlockIdx
    | otherwise =
        fmap maybeOneOrError . runSelectMap id . select $ do
            block <- all_ (esBlocks es)
            guard_ (brHash block ==. val_ phHash)
            return (brIdx block)

-- | Returns blocks starting from given one (including) up to the tip.
getPrivateBlocksAfter
    :: MonadIO m
    => BlockIdx -> DBT t w m (OldestFirst [] PrivateBlockHeader)
getPrivateBlocksAfter idx =
    fmap OldestFirst . runSelectMap pbHeaderFromRow . select $
        orderBy_ (asc_ . brIdx) $
        filter_ (\block -> brIdx block >. val_ idx) $
        all_ (esBlocks es)

getPrivateBlocksAfterHash
    :: MonadIO m
    => PrivateHeaderHash -> DBT t w m (Maybe $ OldestFirst [] PrivateBlockHeader)
getPrivateBlocksAfterHash phHash = do
    midx <- getPrivateBlockIdxByHash phHash
    forM @Maybe midx getPrivateBlocksAfter

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

    lift . rewrapAlreadyExists (BlockWithIndexDomain bid) $
        runInsert . insert (esBlocks es) $ insertExpression $
            BlockRow
            { brIdx = val_ bid
            , brHash = val_ $ hash hdr
            , brCreationTime = currentTimestampUtc_
            , brPrevHash = val_ prev
            , brAtgDelta = val_ trueDelta
            , brMerkleRoot = val_ root
            , brMerkleTree = val_ $ getEmptyMerkleTree tree
            }

    for_ txs' $ \(txIdx, txId) -> lift $ do
        runUpdate $ update
            (esTransactions es)
            (\tx -> [ trIdx tx <-. val_ (TxBlockIdx txIdx) ])
            (\tx -> pk tx ==. valPk_ txId)
        runInsert . insert (esBlockTxs es) . insertValue $
            txId <:-:> bid

    return hdr

createSignedSubmission
    :: DBM m
    => SignedSubmission -> DBT 'WithinTx 'Writing m (Id SignedSubmission)
createSignedSubmission sigSub = do
    let
        submission     = sigSub^.ssSubmission

        student        = submission^.sStudentId
        submissionHash = submission^.idOf
        assignmentId   = submission^.sAssignmentHash

    _ <- existsStudent student      `assertExists`      StudentDomain   student
    _ <- getAssignment assignmentId `assertJustPresent` AssignmentDomain assignmentId
    _ <- isAssignedToStudent student assignmentId
        `assertExists` StudentAssignmentSubscriptionDomain student assignmentId

    rewrapAlreadyExists (SubmissionDomain submissionHash) $
        runInsert . insert (esSubmissions es) $ insertExpression $
            SubmissionRow
            { srHash = val_ submissionHash
            , srContentsHash = submission^.sContentsHash.to val_
            , srSignature = sigSub^.ssWitness.to val_
            , srCreationTime = currentTimestampUtc_
            , srStudent = valPk_ $ submission^.sStudentId
            , srAssignmentHash = valPk_ $ submission^.sAssignmentHash
            }

    return submissionHash

setStudentAssignment :: DBM m => Id Student -> Id Assignment -> DBT 'WithinTx 'Writing m ()
setStudentAssignment studentId assignmentId = do
    _          <- existsStudent studentId    `assertExists`      StudentDomain    studentId
    assignment <- getAssignment assignmentId `assertJustPresent` AssignmentDomain assignmentId

    let courseId = assignment^.aCourseId

    _ <- existsCourse            courseId `assertExists` CourseDomain                         courseId
    _ <- isEnrolledTo  studentId courseId `assertExists` StudentCourseEnrollmentDomain studentId courseId

    rewrapAlreadyExists (StudentAssignmentSubscriptionDomain studentId assignmentId) $
        runInsert . insert (esStudentAssignments es) . insertValue $
            studentId <:-:> assignmentId

isEnrolledTo :: MonadIO m => Id Student -> Id Course -> DBT t w m Bool
isEnrolledTo studentId' courseId' =
    checkExists $ do
        studentId :-: courseId <- all_ (esStudentCourses es)
        guard_ (studentId ==. valPk_ studentId')
        guard_ (courseId ==. valPk_ courseId')

isAssignedToStudent
    :: MonadIO m
    => Id Student -> Id Assignment -> DBT t w m Bool
isAssignedToStudent student' assignment' =
    checkExists $ do
        student :-: assignment <- all_ (esStudentAssignments es)
        guard_ (student ==. valPk_ student')
        guard_ (assignment ==. valPk_ assignment')

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
            courses <- runInsertReturning (esCourses es) $ insertExpression $
                -- TODO: 'default_' does not work for SQLite, right?
                -- Instead use https://tathougies.github.io/beam/user-guide/manipulation/insert/#choosing-a-subset-of-columns
                CourseRow{ crId = default_, crDesc = val_ $ cdDesc params }
            return (crId $ oneOrError courses)
        Just courseId -> do
            rewrapAlreadyExists (CourseDomain courseId) $
                runInsert . insert (esCourses es) . insertValue $
                    CourseRow{ crId = courseId, crDesc = cdDesc params }
            return courseId

    runInsert . insert (esSubjects es) $ insertValues $
        cdSubjects params <&> \subj ->
            SubjectRow
            { srId = subj
            , srDesc = ""
            , srCourse = packPk course
            }

    return course

getCourseSubjects :: MonadIO m => Course -> DBT t w m [Subject]
getCourseSubjects course' =
    runSelect . select $ do
        subject <- all_ (esSubjects es)
        guard_ (srCourse subject ==. valPk_ course')
        return (srId subject)

existsCourse :: MonadIO m => Id Course -> DBT t w m Bool
existsCourse = existsWithPk (esCourses es)

existsStudent :: MonadIO m => Id Student -> DBT t w m Bool
existsStudent = existsWithPk (esStudents es)

existsSubmission :: MonadIO m => Id Submission -> DBT t w m Bool
existsSubmission = existsWithPk (esSubmissions es)

createStudent :: DBM m => Student -> DBT t 'Writing m (Id Student)
createStudent student = do
    rewrapAlreadyExists (StudentDomain student) $
        runInsert . insert (esStudents es) . insertValue $
            StudentRow
            { srAddr = student
            }
    return student

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
getAssignment = selectByPk assignmentFromRow (esAssignments es)

getSignedSubmission
    :: MonadIO m
    => Id SignedSubmission -> DBT t w m (Maybe SignedSubmission)
getSignedSubmission = selectByPk submissionFromRow (esSubmissions es)

createTransaction
    :: DBM m
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

getTransaction :: DBM m => Id PrivateTx -> DBT t w m (Maybe PrivateTx)
getTransaction ptid = do
    fmap listToMaybe . runSelectMap privateTxFromRow . select $ do
        privateTx <- related_ (esTransactions es) (valPk_ ptid)
        submission <- related_ (esSubmissions es) (trSubmissionHash privateTx)
        return (privateTx, submission)

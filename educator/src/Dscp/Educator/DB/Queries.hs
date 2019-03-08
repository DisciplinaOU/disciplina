{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Dscp.Educator.DB.Queries
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
         -- * Synonym for MonadSQL
       , DBM

         -- * Utils
       , rewrapAlreadyExists
       , rewrapReferenceGotInvalid

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
       , existsAssignment
       , existsSubmission
       , existsTransaction
       , existsCertificateMeta

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
       , dumpNonChainedTransactions
       , createSignedSubmission
       , setStudentAssignment
       , createCourse
       , createStudent
       , createAssignment
       , createTransaction
       , createCertificate
       , createCertificatePdf
       ) where

import Control.Lens (to)
import Control.Exception.Safe (catchJust)
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime)
import Database.Beam.Postgres (PgJSONB (..))
import GHC.Exts (fromList)
import Loot.Base.HasLens (HasCtx)
import Pdf.Scanner (PDFBody)
import Snowdrop.Util (OldestFirst (..))

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQL.Error (asAlreadyExistsError, asReferenceInvalidError)
import Dscp.DB.SQL.Functions
import Dscp.DB.SQL.Util
import Dscp.Educator.DB.BlockData
import Dscp.Educator.DB.Error
import Dscp.Educator.DB.Instances ()
import Dscp.Educator.DB.Schema
import Dscp.Educator.Launcher.Marker
import Dscp.Resource.Keys
import Dscp.Util
import Dscp.Util.Rethrow

-- | Short alias for @'educatorSchema'@ for convenience.
es :: DatabaseSettings be EducatorSchema
es = educatorSchema

-- | Catch "unique" constraint violation and rethrow specific error.
rewrapAlreadyExists :: MonadRethrow m => DomainErrorItem -> m a -> m a
rewrapAlreadyExists err =
    rethrowJust $ \e -> asAlreadyExistsError e $> AlreadyPresentError err

assertExists :: MonadThrow m => m Bool -> DomainErrorItem -> m ()
assertExists action = assertJustPresent (bool Nothing (Just ()) <$> action)

assertJustPresent :: MonadThrow m => m (Maybe a) -> DomainErrorItem -> m a
assertJustPresent action err =
    action >>= maybe (throwM $ AbsentError err) pure

-- | Catch "foreign" constraint violation and rethrow specific error.
rewrapReferenceGotInvalid :: (MonadRethrow m, Exception e) => e -> m a -> m a
rewrapReferenceGotInvalid err =
    rethrowJust $ \e -> asReferenceInvalidError e $> err

type DBM m = (MonadIO m, MonadCatch m)

-- | How can a student get a list of courses?
getStudentCourses :: MonadIO m => Id Student -> DBT t m [Id Course]
getStudentCourses student' =
    runSelect . select $ do
        student :-: CourseRowId course <- all_ (esStudentCourses es)
        guard_ (student ==. valPk_ student')
        return course

-- | How can a student enroll to a course?
enrollStudentToCourse :: DBM m => Id Student -> Id Course -> DBT 'WithinTx m ()
enrollStudentToCourse student course = do
    -- TODO: try foreign constraints check
    existsCourse  course  `assertExists` CourseDomain  course
    existsStudent student `assertExists` StudentDomain student

    rewrapAlreadyExists (StudentCourseEnrollmentDomain student course) $
        runInsert . insert (esStudentCourses es) $
            insertValues [student <:-:> course]

-- | How can a student get a list of his current course assignments?
getStudentAssignments
    :: MonadIO m
    => Id Student -> Id Course -> DBT t m [Assignment]
getStudentAssignments student' course' = do
    runSelectMap assignmentFromRow . select $ do
        assignment <- all_ (esAssignments es)
        link_ (esStudentAssignments es) (valPk_ student' :-: pk_ assignment)
        guard_ (arCourse assignment ==. val_ (CourseRowId course'))
        return assignment

-- | How can a student submit a submission for assignment?
submitAssignment
    :: DBM m
    => SignedSubmission -> DBT 'WithinTx m (Id SignedSubmission)
submitAssignment = createSignedSubmission

-- How can a student see his grades for course assignments?
getGradesForCourseAssignments
    :: MonadIO m
    => Id Student -> Id Course -> DBT t m [PrivateTx]
getGradesForCourseAssignments student' course' = do
    runSelectMap (PrivateTxGrade . privateGradeFromRow) . select $ do
        privateGrade <- all_     (esGrades      es)
        submission   <- related_ (esSubmissions es) (grSubmission privateGrade)
        assignment   <- related_ (esAssignments es) (srAssignment submission)
        guard_ (srStudent submission ==. valPk_ student')
        guard_ (arCourse  assignment ==. valPk_ course')
        return (privateGrade, submission)

-- | How can a student receive transactions with Merkle proofs which contain info about his grades and assignments?
getStudentTransactions :: MonadIO m => Id Student -> DBT t m [PrivateTx]
getStudentTransactions student' = do
    runSelectMap (PrivateTxGrade . privateGradeFromRow) . select $ do
        privateTx  <- all_     (esGrades      es)
        submission <- related_ (esSubmissions es) (grSubmission privateTx)
        guard_ (srStudent submission ==. valPk_ student')
        return (privateTx, submission)

data GetProvenStudentTransactionsFilters = GetProvenStudentTransactionsFilters
    { pfCourse     :: Maybe Course
    , pfStudent    :: Maybe Student
    , pfAssignment :: Maybe (Hash Assignment)
    , pfSince      :: Maybe Timestamp
    } deriving (Show, Generic)

deriving instance Default GetProvenStudentTransactionsFilters

-- | Returns list of transaction blocks along with block-proof of a student since given moment.
getProvenStudentTransactions
    :: forall m.
       DBM m
    => GetProvenStudentTransactionsFilters
    -> DBT 'WithinTx m [(PrivateHeaderHash, EmptyMerkleProof PrivateTx, [PrivateTx])]
getProvenStudentTransactions filters = do
    -- Contains `(tx, idx, blockId)` map.
    txsBlockList <- getTxsBlockMap

    -- Bake `blockId -> [(tx, idx)]` map.
    let txsBlockMap = groupToAssocWith txsBlockList
                  -- TODO: remove if transaction order is not needed to be preserved
                  <&> (<&> reverse)

    results <- forM txsBlockMap $ \(blockId, transactions) -> do
        (blockHash, tree) <- getMerkleTreeAndHash blockId

        let indices = Set.fromList $
                      zipWith const [unTxWithinBlockIdx firstBlockTxIdx ..] transactions
            pruned  = mkEmptyMerkleProof tree indices

        return (blockHash, pruned, transactions)

    return [(blockHash, proof, txs) | (blockHash, Just proof, txs) <- results]
  where
    groupToAssocWith :: Ord k => [(k, v)] -> [(k, [v])]
    groupToAssocWith =
        Map.toList . foldl' push Map.empty
      where
        push acc a = Map.insertWith (++) (fst a) [snd a] acc

    getTxsBlockMap :: DBT t m [(BlockIdx, PrivateTx)]
    getTxsBlockMap =
        runSelectMap rearrange . select $
            orderBy_ (\(_bi, (tx, _sub)) ->
                asc_ (grIdx tx)) $ do
                    txId :-: BlockRowId blockIdx <- all_ (esBlockTxs es)
                    privateGrade <- related_ (esGrades       es) (GradeRowId txId)
                    submission   <- related_ (esSubmissions  es) (grSubmission privateGrade)

                    guard_ (grIdx privateGrade /=. val_ TxInMempool)

                    whenJust (pfSince filters) $ \since ->
                        guard_ (grCreationTime privateGrade >=. val_ since)

                    whenJust (pfCourse filters) $ \course -> do
                        assignment <- related_ (esAssignments es) (srAssignment submission)
                        guard_ (arCourse assignment ==. valPk_ course)

                    guard_ $ filterMatchesPk_ (pfStudent    filters) (srStudent    submission)
                    guard_ $ filterMatchesPk_ (pfAssignment filters) (srAssignment submission)

                    return (blockIdx, (privateGrade, submission))
      where
        rearrange (bi, (tx, sub)) = (bi, PrivateTxGrade $ privateGradeFromRow (tx, sub))

    getMerkleTreeAndHash :: BlockIdx -> DBT t m (PrivateHeaderHash, EmptyMerkleTree PrivateTx)
    getMerkleTreeAndHash blockIdx =
        nothingToThrow (AbsentError $ BlockWithIndexDomain blockIdx) =<<
        selectByPk (\row -> (brHash row, brMerkleTree row)) (esBlocks es) blockIdx

getAllNonChainedTransactions :: MonadIO m => DBT t m [PrivateTx]
getAllNonChainedTransactions =
    runSelectMap (PrivateTxGrade . privateGradeFromRow) . select $ do
        privateGrade <- all_     (esGrades      es)
        submission   <- related_ (esSubmissions es) (grSubmission privateGrade)
        guard_ (grIdx privateGrade ==. val_ TxInMempool)
        return (privateGrade, submission)

genesisBlockIdx :: BlockIdx
genesisBlockIdx = 0

getLastBlockIdAndIdx
    :: (MonadIO m, HasCtx ctx m '[KeyResources EducatorNode])
    => DBT t m (Hash PrivateBlockHeader, BlockIdx)
getLastBlockIdAndIdx = do
    author <- ourAddress @EducatorNode
    res <- runSelect . select $
        limit_ 1 $
        orderBy_ (desc_ . snd) $ do
            block <- all_ (esBlocks es)
            return (brHash block, brIdx block)
    if null res
       then return (genesisHeaderHash author, genesisBlockIdx)
       else return (oneOrError res)

getPrivateBlock
    :: MonadIO m
    => BlockIdx -> DBT t m (Maybe PrivateBlockHeader)
getPrivateBlock = selectByPk pbHeaderFromRow (esBlocks es)

-- TODO [DSCP-384]: requires index on Blocks.hash
getPrivateBlockIdxByHash
    :: (MonadIO m, HasCtx ctx m '[KeyResources EducatorNode])
    => PrivateHeaderHash -> DBT t m (Maybe BlockIdx)
getPrivateBlockIdxByHash phHash = do
    author <- ourAddress @EducatorNode
    if phHash == genesisHeaderHash author
       then pure $ Just genesisBlockIdx
       else fmap maybeOneOrError . runSelect $ select query
  where
    query = do
        block <- all_ (esBlocks es)
        guard_ (brHash block ==. val_ phHash)
        return (brIdx block)

-- | Returns blocks starting from given one (excluding) up to the tip.
getPrivateBlocksAfter
    :: MonadIO m
    => BlockIdx -> DBT t m (OldestFirst [] PrivateBlockHeader)
getPrivateBlocksAfter idx =
    fmap OldestFirst . runSelectMap pbHeaderFromRow . select $
        orderBy_ (asc_ . brIdx) $
        filter_ (\block -> brIdx block >. val_ idx) $
        all_ (esBlocks es)

getPrivateBlocksAfterHash
    :: (MonadIO m, HasCtx ctx m '[KeyResources EducatorNode])
    => PrivateHeaderHash -> DBT t m (Maybe $ OldestFirst [] PrivateBlockHeader)
getPrivateBlocksAfterHash phHash = do
    midx <- getPrivateBlockIdxByHash phHash
    forM @Maybe midx getPrivateBlocksAfter

firstBlockTxIdx :: TxWithinBlockIdx
firstBlockTxIdx = 0

createPrivateBlock
    :: (DBM m, HasCtx ctx m '[KeyResources EducatorNode])
    => [PrivateTx]
    -> Maybe ATGDelta
    -> DBT 'WithinTx m (Maybe PrivateBlockHeader)
createPrivateBlock txs delta = runMaybeT $ do
    (prev, idx) <- lift getLastBlockIdAndIdx

    let tree = fromList txs
        root = getMerkleRoot tree

        txs' = zip [firstBlockTxIdx ..] (getId <$> txs)
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
        runUpdate_ $ update
            (esGrades es)
            (\tx -> [ grIdx tx <-. val_ (TxBlockIdx txIdx) ])
            (\tx -> pk_ tx ==. GradeRowId (valPk_ txId))
        runInsert . insert (esBlockTxs es) . insertValue $
            txId <:-:> bid

    return hdr

dumpNonChainedTransactions
    :: (DBM m, HasCtx ctx m '[KeyResources EducatorNode])
    => Maybe ATGDelta
    -> DBT 'WithinTx m (Maybe PrivateBlockHeader)
dumpNonChainedTransactions atgDelta = do
    txs <- getAllNonChainedTransactions
    createPrivateBlock txs atgDelta

createSignedSubmission
    :: DBM m
    => SignedSubmission -> DBT 'WithinTx m (Id SignedSubmission)
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

    time <- toTimestamp <$> liftIO getCurrentTime

    rewrapAlreadyExists (SubmissionDomain submissionHash) $
        runInsert . insert (esSubmissions es) $ insertValue $
            SubmissionRow
            { srHash = submissionHash
            , srContentsHash = submission^.sContentsHash
            , srSignature = sigSub^.ssWitness
            , srCreationTime = time
            , srStudent = packPk $ submission^.sStudentId
            , srAssignment = packPk $ submission^.sAssignmentHash
            }

    return submissionHash

setStudentAssignment :: DBM m => Id Student -> Id Assignment -> DBT 'WithinTx m ()
setStudentAssignment studentId assignmentId = do
    _          <- existsStudent studentId    `assertExists`      StudentDomain    studentId
    assignment <- getAssignment assignmentId `assertJustPresent` AssignmentDomain assignmentId

    let courseId = assignment^.aCourseId

    _ <- existsCourse            courseId `assertExists` CourseDomain                         courseId
    _ <- isEnrolledTo  studentId courseId `assertExists` StudentCourseEnrollmentDomain studentId courseId

    rewrapAlreadyExists (StudentAssignmentSubscriptionDomain studentId assignmentId) $
        runInsert . insert (esStudentAssignments es) . insertValue $
            studentId <:-:> assignmentId

isEnrolledTo :: MonadIO m => Id Student -> Id Course -> DBT t m Bool
isEnrolledTo studentId courseId =
    checkExists $ link_ (esStudentCourses es) (valPk_ studentId :-: valPk_ courseId)

isAssignedToStudent
    :: MonadIO m
    => Id Student -> Id Assignment -> DBT t m Bool
isAssignedToStudent student assignment =
    checkExists $ link_ (esStudentAssignments es) (valPk_ student :-: valPk_ assignment)

data CourseDetails = CourseDetails
    { cdCourseId :: Maybe Course
    , cdDesc     :: ItemDesc
    , cdSubjects :: [Id Subject]
    } deriving (Show, Generic)

-- | Course without any specific information. For testing purposes.
nullCourse :: CourseDetails
nullCourse = CourseDetails{ cdCourseId = Nothing, cdDesc = "", cdSubjects = [] }

-- | Course with specific id but without any other information.
-- For testing purposes.
simpleCourse :: Course -> CourseDetails
simpleCourse i = nullCourse{ cdCourseId = Just i }

createCourse :: DBM m => CourseDetails -> DBT 'WithinTx m (Id Course)
createCourse params = do
    course <- case cdCourseId params of
        Nothing -> do
            nextId <- getNextPrimaryKey (esCourses es)
            runInsert . insert (esCourses es) . insertValue $
                CourseRow{ crId = nextId, crDesc = cdDesc params }
            return nextId
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

getCourseSubjects :: MonadIO m => Course -> DBT t m [Subject]
getCourseSubjects course' =
    runSelect . select $ do
        subject <- all_ (esSubjects es)
        guard_ (srCourse subject ==. valPk_ course')
        return (srId subject)

existsCourse :: MonadIO m => Id Course -> DBT t m Bool
existsCourse = existsWithPk (esCourses es)

existsStudent :: MonadIO m => Id Student -> DBT t m Bool
existsStudent = existsWithPk (esStudents es)

existsAssignment :: MonadIO m => Id Assignment -> DBT t m Bool
existsAssignment = existsWithPk (esAssignments es)

existsSubmission :: MonadIO m => Id Submission -> DBT t m Bool
existsSubmission = existsWithPk (esSubmissions es)

existsTransaction :: MonadIO m => Id PrivateTx -> DBT t m Bool
existsTransaction = existsWithPk (esTransactions es)

existsCertificateMeta :: MonadIO m => Id CertificateFullInfo -> DBT t m Bool
existsCertificateMeta = existsWithPk (esCertificates es)

createStudent :: DBM m => Student -> DBT t m (Id Student)
createStudent student = do
    rewrapAlreadyExists (StudentDomain student) $
        runInsert . insert (esStudents es) . insertValue $
            StudentRow
            { srAddr = student
            }
    return student

createAssignment :: DBM m => Assignment -> DBT 'WithinTx m (Id Assignment)
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
    => Id Assignment -> DBT t m (Maybe Assignment)
getAssignment = selectByPk assignmentFromRow (esAssignments es)

getSignedSubmission
    :: MonadIO m
    => Id SignedSubmission -> DBT t m (Maybe SignedSubmission)
getSignedSubmission = selectByPk submissionFromRow (esSubmissions es)

createTransaction
    :: DBM m
    => PrivateTx -> DBT 'WithinTx m (Id PrivateTx)
createTransaction trans = do
    let ptid :: Id PrivateTx = trans^.idOf
    rewrapAlreadyExists (TransactionDomain ptid) $ do
        case trans of
            PrivateTxGrade grade -> do
                let subHash = grade^.ptSignedSubmission.idOf

                _ <- getSignedSubmission subHash `assertJustPresent`
                    SubmissionDomain subHash

                runInsert . insert (esGrades es) . insertValue $
                    GradeRow
                    { grHash         = trans^.idOf.to TransactionRowId
                    , grSubmission   = packPk subHash
                    , grGrade        = grade^.ptGrade
                    , grCreationTime = grade^.ptTime
                    , grIdx          = TxInMempool
                    }
            PrivateTxCertification cert -> do
              createCertificate (cert^.sgMessage)

        runInsert . insert (esTransactions es) . insertValue $
            TransactionRow
            { trId   = ptid
            , trType = trans^.to getPrivateTxType
            }

    return ptid

createCertificate
    :: DBM m
    => CertificateFullInfo -> DBT t m ()
createCertificate meta =
    rewrapAlreadyExists (CertificateDomain (getId meta)) $
        runInsert . insert (esCertificates es) $ insertValue
            CertificateRow
            { crHash = hash meta
            , crInfo = PgJSONB meta
            }

createCertificatePdf
    :: DBM m
    => CertificateFullInfo -> PDFBody -> DBT t m ()
createCertificatePdf meta pdf =
    rewrapAlreadyExists (CertificateDomain (getId meta)) $
        runInsert . insert (esCertificatesPdf es) $ insertValue
            CertificatePdfRow
            { cprHash = hash meta
            , cprPdf  = pdf
            }



getTransaction :: DBM m => Id PrivateTx -> DBT t m (Maybe PrivateTx)
getTransaction ptid = do
    fmap listToMaybe . runSelectMap (PrivateTxGrade . privateGradeFromRow) . select $ do
        privateGrade <- related_ (esGrades      es) (GradeRowId $ valPk_ ptid)
        submission   <- related_ (esSubmissions es) (grSubmission privateGrade)
        return (privateGrade, submission)

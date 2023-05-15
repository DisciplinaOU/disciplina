{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes    #-}

module Dscp.Educator.Web.Educator.Queries
    ( EducatorGetAssignmentsFilters (..)
    , EducatorGetSubmissionsFilters (..)
    -- , educatorRemoveStudent
    -- , educatorGetStudents
    -- , educatorGetCourses
    -- , educatorGetCourse
    -- , educatorUnassignFromStudent
    -- , educatorGetAssignment
    -- , educatorGetAssignments
    -- , educatorGetSubmission
    -- , educatorGetSubmissions
    -- , educatorGetGrades
    , educatorPostData
    , educatorAddCertificate
    , educatorUpdateCertificate
    , educatorGetCertificate
    , educatorGetCertificates
    , educatorGetCertificatesWithHeaders
    , educatorMarkBlockValidated
    ) where

import Universum

import Control.Lens (each, at, (?~))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.Aeson (eitherDecode)
import Data.Default (Default)
-- import Data.List (groupBy)
import Data.Time.Clock (getCurrentTime)
import Data.List.NonEmpty ((<|))
import Loot.Log (logDebug)
import Loot.Base.HasLens (lensOf)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import qualified Pdf.FromLatex as Pdf
import Pdf.Scanner (PDFBody (..))
-- import Servant (err501)
import Servant.Util (HList (HNil), PaginationSpec, SortingSpecOf, (.*.))
import Servant.Util.Beam.Postgres (fieldSort, paginate_, sortBy_)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQL
-- import Dscp.Educator.Constants
import Dscp.Educator.DB
import Dscp.Educator.Launcher.Resource (CertificateIssuerResource (..))
import Dscp.Educator.Logic.Certificates
import Dscp.Educator.Web.Educator.Error
import Dscp.Educator.Web.Educator.Types
-- import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Types
import Dscp.Util
import Dscp.Web.Types

----------------------------------------------------------------------------
-- Filters for endpoints
----------------------------------------------------------------------------

data EducatorGetAssignmentsFilters = EducatorGetAssignmentsFilters
    { afCourse  :: Maybe Course
    , afStudent :: Maybe Student
    , afDocType :: Maybe (DocumentType Assignment)
    , afIsFinal :: Maybe IsFinal
    } deriving (Show, Generic)

deriving instance Default EducatorGetAssignmentsFilters

data EducatorGetSubmissionsFilters = EducatorGetSubmissionsFilters
    { sfCourse         :: Maybe Course
    , sfStudent        :: Maybe Student
    , sfDocType        :: Maybe (DocumentType Submission)
    , sfAssignmentHash :: Maybe $ Hash Assignment
    } deriving (Show, Generic)

deriving instance Default EducatorGetSubmissionsFilters

----------------------------------------------------------------------------
-- DB endpoints
----------------------------------------------------------------------------

es :: DatabaseSettings be EducatorSchema
es = educatorSchema

-- educatorRemoveStudent
--     :: MonadEducatorWebQuery m
--     => Student -> DBT t m ()
-- educatorRemoveStudent student = do
--     -- TODO [DSCP-176]: Proper implementation of this method may require
--     -- fundemental rethinking of our database scheme and rewriting many code.
--     -- Should be done soon though.
--     _ <- throwM err501
--     void $ deleteByPk (esStudents es) student

-- educatorGetStudents
--     :: MonadEducatorWebQuery m
--     => Maybe Course -> PaginationSpec -> DBT t m [StudentInfo]
-- educatorGetStudents courseF pagination =
--     runSelectMap StudentInfo . select $
--     paginate_ pagination $ do
--         student <- all_ (esStudents es)
--         whenJust courseF $ \course ->
--             link_ (esStudentCourses es) (pk_ student :-: valPk_ course)
--         return (srAddr student)

-- educatorGetCourses
--     :: DBM m
--     => Maybe Student -> PaginationSpec -> DBT t m [CourseEducatorInfo]
-- educatorGetCourses studentF pagination = do
--     res :: [(Course, ItemDesc, Maybe Subject)] <-
--         runSelect . select $
--         paginate_ pagination query
--     return
--         -- group "subject" fields for the same courses
--         [ CourseEducatorInfo{ ciId, ciDesc, ciSubjects }
--         | course@((ciId, ciDesc, _) : _) <- groupBy ((==) `on` view _1) res
--         , let ciSubjects = course ^.. traversed . _3 . _Just
--         ]
--   where
--     query = do
--         course <- all_ (esCourses es)

        -- whenJust studentF $ \student ->
        --     link_ (esStudentCourses es) (valPk_ student :-: pk_ course)

        -- subject <- leftJoin_ (all_ $ esSubjects es)
        --                       ((`references_` course) . srCourse)
        -- return (crId course, crDesc course, srId subject)

-- educatorGetCourse
--     :: MonadEducatorWebQuery m
--     => Course -> DBT t m CourseEducatorInfo
-- educatorGetCourse courseId = do
--     ciDesc <- selectByPk crDesc (esCourses es) courseId
--                 `assertJust` AbsentError (CourseDomain courseId)
--     ciSubjects <- getCourseSubjects courseId
--     return CourseEducatorInfo{ ciId = courseId, .. }

-- educatorUnassignFromStudent
--     :: MonadEducatorWebQuery m
--     => Student
--     -> Hash Assignment
--     -> DBT t m ()
-- educatorUnassignFromStudent student assignH = do
--     changes <- runDelete $ delete
--         (esStudentAssignments es)
--         (==. val_ (student <:-:> assignH))

--     unless (anyAffected changes) $
--         throwM $ AbsentError (StudentAssignmentSubscriptionDomain student assignH)

--     -- we are not deleting any other info since educator may want it to be preserved
--     -- in case if he wants to assign an assignment again

-- -- | Get exactly one assignment.
-- educatorGetAssignment
--     :: MonadEducatorWebQuery m
--     => Hash Assignment
--     -> DBT t m AssignmentEducatorInfo
-- educatorGetAssignment assignH =
--     selectByPk educatorAssignmentInfoFromRow (esAssignments es) assignH
--         >>= nothingToThrow (AbsentError $ AssignmentDomain assignH)

-- -- | Get educator assignments.
-- educatorGetAssignments
--     :: MonadEducatorWebQuery m
--     => EducatorGetAssignmentsFilters
--     -> PaginationSpec
--     -> DBT t m [AssignmentEducatorInfo]
-- educatorGetAssignments filters pagination =
--     runSelectMap educatorAssignmentInfoFromRow . select $
--     paginate_ pagination $ do
--         assignment <- all_ (esAssignments es)

--         guard_ $ filterMatchesPk_ (afCourse filters) (arCourse assignment)
--         guard_ $ filterMatches_ assignTypeF (arType assignment)
--         whenJust (afDocType filters) $ \docType ->
--             guard_ (eqDocTypeQ docType (arContentsHash assignment))
--         whenJust (afStudent filters) $ \student -> do
--             link_ (esStudentAssignments es) (valPk_ student :-: pk_ assignment)

--         return assignment
--   where
--     assignTypeF = afIsFinal filters ^. mapping (from assignmentTypeRaw)

-- -- | Get exactly one submission.
-- educatorGetSubmission
--     :: MonadEducatorWebQuery m
--     => Hash Submission
--     -> DBT t m SubmissionEducatorInfo
-- educatorGetSubmission subH = do
--     submissions <- runSelectMap educatorSubmissionInfoFromRow . select $ do
--         submission <- related_ (esSubmissions es) (valPk_ subH)
--         mPrivateTx <- leftJoin_ (all_ $ esTransactions es)
--                                 ((`references_` submission) . trSubmission)
--         return (submission, mPrivateTx)
--     listToMaybeWarn submissions
--         >>= nothingToThrow (AbsentError $ SubmissionDomain subH)

-- -- | Get all registered submissions.
-- educatorGetSubmissions
--     :: MonadEducatorWebQuery m
--     => EducatorGetSubmissionsFilters
--     -> PaginationSpec
--     -> DBT t m [SubmissionEducatorInfo]
-- educatorGetSubmissions filters pagination =
--     runSelectMap educatorSubmissionInfoFromRow . select $
--     paginate_ pagination $ do
--         submission <- all_ (esSubmissions es)
--         assignment <- related_ (esAssignments es) (srAssignment submission)

--         guard_ $ filterMatchesPk_ (sfCourse filters) (arCourse assignment)
--         guard_ $ filterMatchesPk_ (sfAssignmentHash filters) (srAssignment submission)
--         whenJust (sfDocType filters) $ \docType ->
--             guard_ (eqDocTypeQ docType (arContentsHash assignment))
--         whenJust (sfStudent filters) $ \student -> do
--             link_ (esStudentAssignments es) (valPk_ student :-: pk_ assignment)

--         mPrivateTx <- leftJoin_ (all_ $ esTransactions es)
--                                 ((`references_` submission) . trSubmission)

--         return (submission, mPrivateTx)

-- educatorGetGrades
--     :: MonadEducatorWebQuery m
--     => Maybe Course
--     -> Maybe Student
--     -> Maybe (Hash Assignment)
--     -> Maybe IsFinal
--     -> DBT t m [GradeInfo]
-- educatorGetGrades courseIdF studentF assignmentF isFinalF =
--     runSelectMap gradeInfoFromRow . select $ do
--         privateTx <- all_ (esTransactions es)
--         submission <- related_ (esSubmissions es) (trSubmission privateTx)
--         assignment <- related_ (esAssignments es) (srAssignment submission)

--         guard_ $ filterMatchesPk_ courseIdF (arCourse assignment)
--         guard_ $ filterMatchesPk_ studentF (srStudent submission)
--         guard_ $ filterMatchesPk_ assignmentF (pk_ assignment)
--         whenJust isFinalF $ \isFinal -> do
--             let assignTypeF = isFinal ^. from assignmentTypeRaw
--             guard_ (arType assignment ==. val_ assignTypeF)

--         return privateTx

educatorPostData
    :: MonadEducatorWebQuery m
    => Entity -> A.Value -> DBT 'WithinTx m ()
educatorPostData entity cData = do
    time <- toTimestamp <$> liftIO getCurrentTime
    let ptx = PrivateTx
            { _ptEntity = entity
            , _ptData = cData
            , _ptTime = time
            }
        txId = getId ptx

    rewrapAlreadyExists (TransactionDomain txId) $
        runInsert . insert (esTransactions es) . insertValue $
            TransactionRow
            { trHash = txId
            , trCreationTime = time
            , trIdx = TxInMempool
            , trEntity = entity
            , trData = PgJSONB cData
            }

mkPdfHashData :: PDFBody -> A.Value
mkPdfHashData (PDFBody body) =
    let pdfHash = hash body
        pdfHash64 = toBase64 pdfHash
    in A.Object $ HM.singleton "pdfHash" $ A.String pdfHash64


headerWithPubTxFromRow :: BlockRow -> HeaderWithPubTx
headerWithPubTxFromRow r@BlockRow{..} = HeaderWithPubTx
    { hwptHeaderHash = brHash
    , hwptHeader     = pbHeaderFromRow r
    , hwptPubTxId    = brPubTxId
    }


-- | Creates a private block consisting of transactions built from certificate grades
-- and generates a PDF certificate with an embedded FairCV (without `PubTxId` embedded yet).
-- Also returns a header of the created block.
educatorAddCertificate
    :: MonadEducatorWeb ctx m
    => CertificateFullInfo -> m (PrivateBlockHeader, PDFBody)
educatorAddCertificate cert = do
    pdfLang         <- view (lensOf @Language)
    pdfLatexPath    <- view (lensOf @Pdf.LatexPath)
    pdfResPath      <- view (lensOf @Pdf.ResourcePath)
    downloadBaseUrl <- view (lensOf @Pdf.DownloadBaseUrl)

    logDebug "Starting creating the certificate"

    certificateIssuerInfo  <- getCertificateIssuerInfo
    pdfRaw <- Pdf.produce pdfLang certificateIssuerInfo cert pdfLatexPath pdfResPath downloadBaseUrl

    logDebug "Certificate PDF created"

    let allDatas = mkPdfHashData pdfRaw <| cfiDatas cert

    transact $ do
        logDebug "Starting adding the certificate data to private chain"
        txs <- addCertificateDatas (cfiMeta cert) allDatas
        logDebug "Transactions created"

        (blkIdx, blkHeader) <-
            createPrivateBlock (toList @(NonEmpty _) txs)
            <&> nothingToPanic "impossible: failed to make non-empty block"

        logDebug "Private block created"

        eAddr <- view $ lensOf @PubAddress
        -- TODO: different names for certificates?
        let faircv = privateBlockToFairCV blkHeader txs eAddr "Watch"

        logDebug "FairCV proof created upon block, embedding..."
        pdf <- embedFairCVToCert' (unReadyFairCV faircv) pdfRaw
        logDebug "Embedded FairCV to PDF, saving certificate to the database..."

        createCertificate (cfiMeta cert) blkIdx pdf
        logDebug "New certificate saved, all done"

        return (blkHeader, pdfRaw)


educatorUpdateCertificate
  :: MonadEducatorWeb ctx m
  => Hash CertificateMeta -> NonEmpty A.Value -> m CertificateWithHeaders
educatorUpdateCertificate certHash datas = do
    pdfLang         <- view (lensOf @Language)
    pdfLatexPath    <- view (lensOf @Pdf.LatexPath)
    pdfResPath      <- view (lensOf @Pdf.ResourcePath)
    downloadBaseUrl <- view (lensOf @Pdf.DownloadBaseUrl)

    logDebug "Fetching and extracting old certificate data"
    (certMeta, oldPdf) <- invoke $ educatorGetCertificateWithMeta certHash
    unless (hash certMeta == certHash) $
        throwM InvalidFormat

    logDebug "Checked that certificate meta yields correct hash"
    (oldFairCV, _) <- maybe (throwM InvalidFormat) pure $ extractFairCVFromCert oldPdf
    oldDatas <- maybe (throwM InvalidFormat) pure $ nonEmpty $ extractDatasFromFairCV oldFairCV
    let newDatas = oldDatas <> datas
        cert = CertificateFullInfo certMeta newDatas

    logDebug "Started re-creating the certificate"
    certificateIssuerInfo  <- getCertificateIssuerInfo
    pdfRaw <- Pdf.produce pdfLang certificateIssuerInfo cert pdfLatexPath pdfResPath downloadBaseUrl

    -- TODO: REMOVE COPY PASTE

    logDebug "Certificate PDF created"

    let allDatas = mkPdfHashData pdfRaw <| datas

    transact $ do
        logDebug "Starting adding the certificate data to private chain"
        txs <- addCertificateDatas (cfiMeta cert) allDatas
        logDebug "Transactions created"

        (blkIdx, blkHeader) <-
            createPrivateBlock (toList @(NonEmpty _) txs)
            <&> nothingToPanic "impossible: failed to make non-empty block"

        logDebug "Private block created"

        eAddr <- view $ lensOf @PubAddress
        -- TODO: different names for certificates?
        let newFairCV = privateBlockToFairCV blkHeader txs eAddr "Watch"
        let mergedFairCV = mergeFairCVs (readyFairCV oldFairCV) newFairCV
              & nothingToPanic "impossible: failed to merge FairCVs non-intersecting in blocks" . eitherToMaybe

        logDebug "FairCV proof created upon block, embedding..."
        pdf <- embedFairCVToCert' (unReadyFairCV mergedFairCV) pdfRaw
        logDebug "Embedded FairCV to PDF, saving certificate to the database..."

        updateCertificatePdf certHash blkIdx pdf
        logDebug "Updated certificate saved, all done"

        blks <- fmap headerWithPubTxFromRow <$> getCertificateBlocks certHash

        return $ CertificateWithHeaders (mkCertificate certMeta) blks

  where
    extractDatasFromFairCV :: FairCV -> [A.Value]
    extractDatasFromFairCV fairCV =
        let FairCV {..} = extractContentsFromFairCV fairCV
            flattenedTxs  = join . toList . fmap (join . fmap tiaVal . toList) $ fcCV
            onlyDatas     = fmap (^. ptData) flattenedTxs

            isPdfHashTx (A.Object hm) = HM.member "pdfHash" hm
            isPdfHashTx _ = False

        in filter (not . isPdfHashTx) onlyDatas


-- | Writes down the Ethereum transaction ID into the private block's row.
-- Also update embedded `FairCVs` in the certificates
educatorMarkBlockValidated
  :: MonadEducatorWebQuery m
  => PrivateHeaderHash
  -> PubTxId
  -> DBT 'WithinTx m Bool
educatorMarkBlockValidated blkHash txId = do
    affectedBlks <- runUpdateReturning $ update
        (esBlocks es)
        (\blk -> brPubTxId blk <-. val_ (Just txId))
        (\blk -> brHash blk ==. val_ blkHash)

    fmap or $ forM affectedBlks $ \blkRow -> do
        let blkIdx = brIdx blkRow

        certs <- runSelect . select $ do
            BlockRowId blkIdx' :-: certHash <- all_ (esCertificateBlocks es)
            guard_ (blkIdx' ==. val_ blkIdx)
            related_ (esCertificates es) certHash

        forM_ certs $ \certRow -> do
             let pdf   = crPdf certRow
                 cHash = crHash certRow
                 updateNecessaryHash fcv = fcv & fcCVL.each.at blkHash.each.tiaTxIdL ?~ txId

             updatedPdf <- nothingToThrow InvalidFormat $
                 mapFairCV updateNecessaryHash pdf
             runUpdate $ update
                 (esCertificates es)
                 (\crt -> crPdf crt <-. val_ updatedPdf)
                 (\crt -> crHash crt ==. val_ cHash)

        return True


getCertificateIssuerInfo
    :: MonadEducatorWeb ctx m
    => m Pdf.CertificateIssuerInfo
getCertificateIssuerInfo = view (lensOf @CertificateIssuerResource) >>= \case
    KnownIssuerInfo info -> return info
    FromServiceIssuerInfo baseUrl authToken -> do
        response <- liftIO $ do
            manager <- newManager tlsManagerSettings
            initialRequest <- parseRequest $ showBaseUrl baseUrl
            let request = initialRequest
                    { requestHeaders =
                        [ ("Authorization", authToken)
                        , ("accept", "application/json")
                        ]
                    , checkResponse = throwStatusErrors
                    }
            responseBody <$> httpLbs request manager
        leftToThrow NonDecodableResponseError $ eitherDecode response
  where
    throwStatusErrors _ res = let sCode = statusCode $ responseStatus res in
        unless (200 <= sCode && sCode < 300) $
            throwM $ NonPositiveStatusCodeError sCode

educatorGetCertificate
    :: MonadEducatorWebQuery m
    => Hash CertificateMeta -> DBT t m PDFBody
educatorGetCertificate = fmap snd . educatorGetCertificateWithMeta


educatorGetCertificateWithMeta
    :: MonadEducatorWebQuery m
    => Hash CertificateMeta -> DBT t m (CertificateMeta, PDFBody)
educatorGetCertificateWithMeta certId =
    selectByPk (\row -> (case crMeta row of PgJSONB m -> m, crPdf row)) (esCertificates es) certId
        >>= nothingToThrow (AbsentError $ CertificateDomain certId)


educatorGetCertificates
    :: MonadEducatorWebQuery m
    => SortingSpecOf Certificate
    -> PaginationSpec
    -> DBT t m [Certificate]
educatorGetCertificates sorting pagination =
    runSelectMap certificateFromRow . select $
    paginate_ pagination $ do
        cert <- sortBy_ sorting sortingSpecApp $ all_ (esCertificates es)
        return (crHash cert, crMeta cert)
  where
    sortingSpecApp CertificateRow{..} =
        fieldSort @"createdAt" (crMeta ->>$. #cmIssueDate) .*.
        fieldSort @"title" (crMeta ->>$. #cmTitle) .*.
        HNil

educatorGetCertificatesWithHeaders
    :: MonadEducatorWeb ctx m
    => SortingSpecOf Certificate
    -> PaginationSpec
    -> DBT t m [CertificateWithHeaders]
educatorGetCertificatesWithHeaders sorting pagination = do
    certificates <- educatorGetCertificates sorting pagination
    headers <- forM certificates $ \Certificate {..} ->
        runSelectMap headerWithPubTxFromRow . select $ orderBy_ (desc_ . brIdx) $ do
            blkIdx :-: certHash <- all_ (esCertificateBlocks es)
            guard_ (certHash ==. valPk_ cId)
            related_ (esBlocks es) blkIdx
    return $ zipWith CertificateWithHeaders certificates headers
  where

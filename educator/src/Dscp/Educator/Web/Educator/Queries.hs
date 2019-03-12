{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Dscp.Educator.Web.Educator.Queries
    ( educatorRemoveStudent
    , educatorGetStudents
    , educatorGetCourses
    , educatorGetCourse
    , educatorUnassignFromStudent
    , educatorGetAssignment
    , educatorGetAssignments
    , educatorGetSubmission
    , educatorGetSubmissions
    , educatorGetGrades
    , educatorPostGrade
    , educatorAddCertificate
    , educatorGetCertificate
    , educatorGetCertificates
    ) where

import Control.Lens (from)
import Data.Aeson (eitherDecode)
import Data.Time.Clock (getCurrentTime)
import Loot.Base.HasLens (lensOf)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import qualified Pdf.FromLatex as Pdf
import Pdf.Scanner (PDFBody (..))
import Database.Beam.Postgres (arrayOf_)
import Servant (err501)
import Servant.Client.Core.Internal.BaseUrl (showBaseUrl)
import Servant.Util (PaginationSpec, SortingSpecOf, HList (HNil), (.*.), FilteringSpecOf, htuple)
import Servant.Util.Beam.Postgres (fieldSort, paginate_, sortBy_, filtersGuard_, manualFilter, matches_, filterOn)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQL
import Dscp.Educator.Constants
import Dscp.Educator.DB
import Dscp.Educator.Launcher.Marker
import Dscp.Educator.Launcher.Resource (CertificateIssuerResource (..))
import Dscp.Educator.Logic.Certificates
import Dscp.Educator.Web.Educator.Error
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Types
import Dscp.Resource.Keys
import Dscp.Util

----------------------------------------------------------------------------
-- DB endpoints
----------------------------------------------------------------------------

es :: DatabaseSettings be EducatorSchema
es = educatorSchema

educatorRemoveStudent
    :: MonadEducatorWebQuery m
    => Student -> DBT t m ()
educatorRemoveStudent student = do
    -- TODO [DSCP-176]: Proper implementation of this method may require
    -- fundemental rethinking of our database scheme and rewriting many code.
    -- Should be done soon though.
    _ <- throwM err501
    void $ deleteByPk (esStudents es) student

educatorGetStudents
    :: MonadEducatorWebQuery m
    => FilteringSpecOf StudentInfo -> PaginationSpec -> DBT t m [StudentInfo]
educatorGetStudents filters pagination =
    runSelectMap StudentInfo . select $
    paginate_ pagination $ do
        student <- all_ (esStudents es)
        filtersGuard_ filters (filteringSpecApp student)
        return (srAddr student)
  where
    filteringSpecApp student = htuple
        ( manualFilter $ \course ->
            link_ (esStudentCourses es) (pk_ student :-: valPk_ course)
        )

educatorGetCourses
    :: DBM m
    => FilteringSpecOf CourseEducatorInfo
    -> PaginationSpec
    -> DBT t m [CourseEducatorInfo]
educatorGetCourses filters pagination =
    runSelectMap educatorCourseInfoFromRow . select $
    paginate_ pagination $ do
        course <- all_ (esCourses es)
        filtersGuard_ filters (filteringSpecApp course)

        let subjectIds = arrayOf_ $ do
                subject <- all_ (esSubjects es)
                guard_ (srCourse subject `references_` course)
                return $ srId subject

        return (crId course, crDesc course, subjectIds)
  where
    filteringSpecApp course = htuple
        ( manualFilter $ \student ->
            link_ (esStudentCourses es) (valPk_ student :-: pk_ course)
        )

educatorGetCourse
    :: MonadEducatorWebQuery m
    => Course -> DBT t m CourseEducatorInfo
educatorGetCourse courseId = do
    ciDesc <- selectByPk crDesc (esCourses es) courseId
                `assertJust` AbsentError (CourseDomain courseId)
    ciSubjects <- getCourseSubjects courseId
    return CourseEducatorInfo{ ciId = courseId, .. }

educatorUnassignFromStudent
    :: MonadEducatorWebQuery m
    => Student
    -> Hash Assignment
    -> DBT t m ()
educatorUnassignFromStudent student assignH = do
    changes <- runDelete $ delete
        (esStudentAssignments es)
        (==. val_ (student <:-:> assignH))

    unless (anyAffected changes) $
        throwM $ AbsentError (StudentAssignmentSubscriptionDomain student assignH)

    -- we are not deleting any other info since educator may want it to be preserved
    -- in case if he wants to assign an assignment again

-- | Get exactly one assignment.
educatorGetAssignment
    :: MonadEducatorWebQuery m
    => Hash Assignment
    -> DBT t m AssignmentEducatorInfo
educatorGetAssignment assignH =
    selectByPk educatorAssignmentInfoFromRow (esAssignments es) assignH
        >>= nothingToThrow (AbsentError $ AssignmentDomain assignH)

-- | Get educator assignments.
educatorGetAssignments
    :: MonadEducatorWebQuery m
    => FilteringSpecOf AssignmentEducatorInfo
    -> PaginationSpec
    -> DBT t m [AssignmentEducatorInfo]
educatorGetAssignments filters pagination =
    runSelectMap educatorAssignmentInfoFromRow . select $
    paginate_ pagination $ do
        assignment <- all_ (esAssignments es)
        guard_ $ matches_ filters (filteringSpecApp assignment)
        return assignment
  where
    filteringSpecApp assignment@AssignmentRow{..} = htuple
        ( filterOn (unpackPk arCourse)
        , manualFilter (studentF assignment)
        , manualFilter $ \isFinal ->
            val_ (isFinal ^. from assignmentTypeRaw) ==. arType
        )

    studentF assignment student =
        existsAny_ $ link_ (esStudentAssignments es) (valPk_ student :-: pk_ assignment)

-- | Get exactly one submission.
educatorGetSubmission
    :: MonadEducatorWebQuery m
    => Hash Submission
    -> DBT t m SubmissionEducatorInfo
educatorGetSubmission subH = do
    submissions <- runSelectMap educatorSubmissionInfoFromRow . select $ do
        submission <- related_ (esSubmissions es) (valPk_ subH)
        mPrivateTx <- leftJoin_ (all_ $ esTransactions es)
                                ((`references_` submission) . trSubmission)
        return (submission, mPrivateTx)
    listToMaybeWarn submissions
        >>= nothingToThrow (AbsentError $ SubmissionDomain subH)

-- | Get all registered submissions.
educatorGetSubmissions
    :: MonadEducatorWebQuery m
    => FilteringSpecOf SubmissionEducatorInfo
    -> PaginationSpec
    -> DBT t m [SubmissionEducatorInfo]
educatorGetSubmissions filters pagination =
    runSelectMap educatorSubmissionInfoFromRow . select $
    paginate_ pagination $ do
        submission <- all_ (esSubmissions es)
        assignment <- related_ (esAssignments es) (srAssignment submission)
        mPrivateTx <- leftJoin_ (all_ $ esTransactions es)
                                ((`references_` submission) . trSubmission)
        filtersGuard_ filters $
            filteringSpecApp assignment submission mPrivateTx
        return (submission, mPrivateTx)
  where
    filteringSpecApp AssignmentRow{..} SubmissionRow{..} mPrivateTx =
        filterOn (unpackPk arCourse) .*.
        filterOn (unpackPk srStudent) .*.
        filterOn arHash .*.
        filterOn (coerceQExpr $ isJust_ mPrivateTx) .*.
        filterOn srCreationTime .*.
        HNil

educatorGetGrades
    :: MonadEducatorWebQuery m
    => FilteringSpecOf GradeInfo
    -> DBT t m [GradeInfo]
educatorGetGrades filters =
    runSelectMap gradeInfoFromRow . select $ do
        privateTx <- all_ (esTransactions es)
        submission <- related_ (esSubmissions es) (trSubmission privateTx)
        assignment <- related_ (esAssignments es) (srAssignment submission)
        guard_ $ matches_ filters $
            filteringSpecApp assignment submission privateTx
        return privateTx
  where
    filteringSpecApp AssignmentRow{..} SubmissionRow{..} TransactionRow{..} = htuple
        ( filterOn (unpackPk arCourse)
        , filterOn (unpackPk srStudent)
        , filterOn arHash
        , manualFilter $ \isFinal ->
            val_ (isFinal ^. from assignmentTypeRaw) ==. arType
        , filterOn trCreationTime
        )

educatorPostGrade
    :: MonadEducatorWebQuery m
    => Hash Submission -> Grade -> DBT 'WithinTx m ()
educatorPostGrade subH grade = do
    time <- toTimestamp <$> liftIO getCurrentTime
    sigSub <- getSignedSubmission subH
        `assertJust` AbsentError (SubmissionDomain subH)

    let ptx = PrivateTx
            { _ptSignedSubmission = sigSub
            , _ptTime = time
            , _ptGrade = grade
            }
        txId = getId ptx

    rewrapAlreadyExists (TransactionDomain txId) $
        runInsert . insert (esTransactions es) . insertValue $
            TransactionRow
            { trHash = txId
            , trGrade = grade
            , trCreationTime = time
            , trIdx = TxInMempool
            , trSubmission = packPk subH
            }

-- | Creates a private block consisting of transactions built from certificate grades
-- and generates a PDF certificate with embedded FairCV describing that block.
educatorAddCertificate
    :: MonadEducatorWeb ctx m
    => CertificateFullInfo -> m PDFBody
educatorAddCertificate cert = do
    pdfLatexPath    <- view (lensOf @Pdf.LatexPath)
    pdfResPath      <- view (lensOf @Pdf.ResourcePath)
    downloadBaseUrl <- view (lensOf @Pdf.DownloadBaseUrl)

    certificateIssuerInfo  <- getCertificateIssuerInfo
    pdfRaw@ (PDFBody body) <- Pdf.produce RU certificateIssuerInfo cert pdfLatexPath pdfResPath downloadBaseUrl

    let pdfHash = hash body

    transact $ do
        txs <- addCertificateGrades pdfHash (cfiMeta cert) (cfiGrades cert)
        blkHeader <-
            createPrivateBlock (toList @(NonEmpty _) txs) Nothing
            <&> nothingToPanic "impossible: failed to make non-empty block"
        -- This private block will be added to the public chain on itself shortly

        eAddr <- ourAddress @EducatorNode
        let sName = unItemDesc . cmStudentName $ cfiMeta cert
        let faircv = privateBlockToFairCV blkHeader txs eAddr (defCertStudent, sName)
        pdf <- embedFairCVToCert (unReadyFairCV faircv) pdfRaw

        createCertificate (cfiMeta cert) pdf

        return pdf

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
educatorGetCertificate certId =
    selectByPk crPdf (esCertificates es) certId
        >>= nothingToThrow (AbsentError $ CertificateDomain certId)

educatorGetCertificates
    :: MonadEducatorWebQuery m
    => SortingSpecOf Certificate
    -> PaginationSpec
    -> DBT t m [Certificate]
educatorGetCertificates sorting pagination =
    runSelectMap certificateFromRow . select $
    paginate_ pagination $ do
        certificate <-
            sortBy_ sorting sortingSpecApp $ do
            all_ (esCertificates es)
        return (crHash certificate, crMeta certificate)
  where
    sortingSpecApp CertificateRow{..} =
        fieldSort @"createdAt" (crMeta ->>$. #cmIssueDate) .*.
        fieldSort @"studentName" (crMeta ->>$. #cmStudentName) .*.
        HNil

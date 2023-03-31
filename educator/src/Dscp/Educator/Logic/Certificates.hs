module Dscp.Educator.Logic.Certificates
    ( embedFairCVToCert
    , embedFairCVToCert'
    , extractFairCVFromCert
    , mapFairCV
    , addCertificateDatas
    ) where

import Universum

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (UTCTime (..))
import qualified Pdf.Scanner as Pdf
import qualified Text.Show

import Dscp.Core
import Dscp.DB.SQL
import Dscp.Educator.DB.Queries
import Dscp.Util

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data FailedToBuildCertificate = FailedToBuildCertificate

instance Show FailedToBuildCertificate where
    show _ = "Failed to build certificate"

instance Exception FailedToBuildCertificate

----------------------------------------------------------------------------
-- Functions
----------------------------------------------------------------------------

-- | Make a fake submission for certificate grade.
-- Has to be unique for each transaction, or getting equal private
-- transactions becomes too easy for the user.
-- randomCertSubmission :: Hash Raw -> SignedSubmission
-- randomCertSubmission contentsHash = do
--     makeSignedSubmission
--         defCertStudentSk
--         (hash defCertAssignment)
--         contentsHash

-- | Lift a grade to private transaction.
certDataToPrivateTx
    :: CertificateMeta
    -> A.Value
    -> PrivateTx
certDataToPrivateTx meta cData =
    let issueTime = toTimestamp $ dayToTime (cmIssueDate meta)
        entity = cmEntity meta
    in PrivateTx
        { _ptEntity = entity
        , _ptData = cData
        , _ptTime = issueTime
        }
  where
    -- this way the "day" part of a timestamp remains the same in all time zones
    midDay = 86400 / 2
    dayToTime day = UTCTime day midDay

-- ensureCertSubmissionExists
--     :: (MonadIO m, MonadCatch m)
--     => DBT 'WithinTx m ()
-- ensureCertSubmissionExists = do
--     unlessM (existsCourse defCertCourse) $
--         void $ createCourse (simpleCourse defCertCourse)

--     unlessM (existsStudent defCertStudent) $
--         void $ createStudent defCertStudent

--     unlessM (isEnrolledTo defCertStudent defCertCourse) $
--         void $ enrollStudentToCourse defCertStudent defCertCourse

--     unlessM (existsAssignment $ getId defCertAssignment) $
--         void $ createAssignment defCertAssignment

--     unlessM (isAssignedToStudent defCertStudent (getId defCertAssignment)) $
--         void $ setStudentAssignment defCertStudent (getId defCertAssignment)

-- | Add a certificate grade to database.
addCertificateDatas
    :: (MonadIO m, MonadCatch m, Traversable t)
    => CertificateMeta -> (t A.Value) -> DBT 'WithinTx m (t PrivateTx)
addCertificateDatas meta cDatas = forM cDatas $ \cData -> do
    let tx = certDataToPrivateTx meta cData
    tx <$ createTransaction tx

-- | Build a certificate with embedded FairCV.
embedFairCVToCert :: FairCV -> Pdf.PDFBody -> Maybe Pdf.PDFBody
embedFairCVToCert faircv pdf =
    Pdf.inject (Pdf.MaxSearchLength Nothing) faircvEncoded pdf
  where
    faircvEncoded = LBS.toStrict $ A.encode faircv

-- | Build a certificate with embedded FairCV, throws an exception
-- if fails.
embedFairCVToCert'
    :: (MonadThrow m)
    => FairCV -> Pdf.PDFBody -> m Pdf.PDFBody
embedFairCVToCert' =
    nothingToThrow FailedToBuildCertificate ... embedFairCVToCert

-- | Extract a FairCV from a certificate, if it is embedded inside.
extractFairCVFromCert :: Pdf.PDFBody -> Maybe (FairCV, Pdf.PDFBody)
extractFairCVFromCert pdf = do
    (bs, pdfRaw) <- Pdf.unInject (Pdf.MaxSearchLength Nothing) pdf
    res <- A.decodeStrict bs
    return (res, pdfRaw)

-- | Applies a function to the encoded FairCV and saves the result back
mapFairCV :: (FairCV -> FairCV) -> Pdf.PDFBody -> Maybe Pdf.PDFBody
mapFairCV f = extractFairCVFromCert >=> uncurry embedFairCVToCert . first f

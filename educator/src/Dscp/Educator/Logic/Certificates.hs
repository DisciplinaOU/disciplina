module Dscp.Educator.Logic.Certificates
    ( certGradeToPrivateTx
    , embedFairCVToCert
    , addCertificateGrades
    ) where

import Control.Monad.State (lift)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (UTCTime (..))
import qualified Pdf.Scanner as Pdf
import qualified Text.Show

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQL
import Dscp.Educator.Constants
import Dscp.Educator.DB.Queries
import Dscp.Educator.Logic.Submission
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
randomCertSubmission :: Hash Raw -> SignedSubmission
randomCertSubmission contentsHash = do
    makeSignedSubmission
        defCertStudentSk
        (hash defCertAssignment)
        contentsHash

-- | Lift a grade to private transaction.
certGradeToPrivateTx
    :: SignedSubmission
    -> CertificateMeta
    -> CertificateGrade
    -> PrivateTx
certGradeToPrivateTx submission meta CertificateGrade{..} =
    let issueTime = toTimestamp $ dayToTime (cmIssueDate meta)
    in PrivateTx
        { _ptSignedSubmission = submission
        , _ptGrade = cgGrade
        , _ptTime = issueTime
        }
  where
    -- this way the "day" part of a timestamp remains the same in all time zones
    midDay = 86400 / 2
    dayToTime day = UTCTime day midDay

ensureCertSubmissionExists
    :: (MonadIO m, MonadCatch m)
    => DBT 'WithinTx m ()
ensureCertSubmissionExists = do
    unlessM (existsCourse defCertCourse) $
        void $ createCourse (simpleCourse defCertCourse)

    unlessM (existsStudent defCertStudent) $
        void $ createStudent defCertStudent

    unlessM (isEnrolledTo defCertStudent defCertCourse) $
        void $ enrollStudentToCourse defCertStudent defCertCourse

    unlessM (existsAssignment $ getId defCertAssignment) $
        void $ createAssignment defCertAssignment

    unlessM (isAssignedToStudent defCertStudent (getId defCertAssignment)) $
        void $ setStudentAssignment defCertStudent (getId defCertAssignment)

-- | Add a certificate grade to database.
addCertificateGrades
    :: (MonadIO m, MonadCatch m, Traversable t)
    => Hash Raw -> CertificateMeta -> (t CertificateGrade) -> DBT 'WithinTx m (t PrivateTx)
addCertificateGrades rawHash meta grades = do
    ensureCertSubmissionExists

    flip evalStateT True $ do
        forM grades $ \grade -> do
            injectHash <- get
            put False

            lift $ do
                submission <- (if injectHash then return rawHash else liftIO randomHash)
                    <&> randomCertSubmission

                void $ createSignedSubmission submission

                let tx = certGradeToPrivateTx submission meta grade
                tx <$ createTransaction tx

-- | Build a certificate with embedded FairCV.
embedFairCVToCert
    :: (MonadThrow m)
    => FairCV -> Pdf.PDFBody -> m Pdf.PDFBody
embedFairCVToCert faircv pdf = do
    let faircvEncoded = LBS.toStrict $ A.encode faircv
    Pdf.inject (Pdf.MaxSearchLength Nothing) faircvEncoded pdf
        & nothingToThrow FailedToBuildCertificate

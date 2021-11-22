
module Dscp.Core.Foundation.Educator.Certificate
    where

import Data.Time.Calendar (Day (..))

import Dscp.Core.Foundation.Educator.Grade
import Dscp.Core.Foundation.Educator.ItemDesc
import Dscp.Crypto
import Dscp.Util

data EducationForm = Fulltime | Parttime | Fullpart
    deriving (Show, Eq, Ord, Generic, Enum, Bounded)

instance Buildable EducationForm where
    build = show

-- | Datatype which contains all the info about certificate. This
-- datatype represents a request body for 'AddCertificate' endpoint.
data CertificateFullInfo = CertificateFullInfo
    { cfiMeta   :: CertificateMeta
    , cfiGrades :: [CertificateGrade]
    } deriving (Show, Eq, Ord, Generic)

instance Buildable CertificateFullInfo where
    build CertificateFullInfo {..} =
        "{ meta = "+|cfiMeta|+
        ", grades = "+|listF cfiGrades|+" }"

-- | Datatype which contains information about the grade which
-- gets included into the certificate.
data CertificateGrade = CertificateGrade
    { cgSubject :: ItemDesc
    , cgLang    :: Language
    , cgHours   :: Int
    , cgCredits :: Maybe Int
    , cgScale   :: GradingScale
    , cgGrade   :: Grade
    } deriving (Show, Eq, Ord, Generic)

instance Buildable CertificateGrade where
    build CertificateGrade {..} =
        "{ subject = "+|cgSubject|+
        ", hours = "+|cgHours|+
        ", credits = "+|cgCredits|+
        ", grade = "+|cgGrade|+" }"

-- | Datatype containing information about Educator which issued
-- the certificate, required in order to render a certificate.
data CertificateIssuerInfo = CertificateIssuerInfo
    { ciiName    :: ItemDesc
    , ciiWebsite :: ItemDesc
    , ciiId      :: Text
    } deriving (Show, Eq, Ord, Generic)

data Language = EN | RU
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Datatype containing info about a certificate issued by Educator.
data CertificateMeta = CertificateMeta
    { cmStudentName      :: !ItemDesc
    , cmStudentBirthDate :: !Day
    , cmStartYear        :: !Word16
    , cmEndYear          :: !Word16
    , cmEducationForm    :: !EducationForm
    , cmNumber           :: !Natural
    , cmIssueDate        :: !Day
    , cmTitle            :: !ItemDesc
    , cmMajor            :: !ItemDesc
    , cmSpecialization   :: !(Maybe ItemDesc)
    } deriving (Show, Eq, Ord, Generic)

instance Buildable CertificateMeta where
    build CertificateMeta {..} =
        "{ studentName = "+|cmStudentName|+
        ", studentBirthDate = "+|cmStudentBirthDate|+
        ", startYear = "+|cmStartYear|+
        ", endYear = "+|cmEndYear|+
        ", educationForm = "+|cmEducationForm|+
        ", number = "+|toInteger cmNumber|+
        ", issueDate = "+|cmIssueDate|+
        ", title = "+|cmTitle|+
        ", major = "+|cmMajor|+
        ", specialization = "+|cmSpecialization|+" }"

-- | Datatype which combines certificate meta with its ID.
data Certificate = Certificate
    { cId   :: Hash CertificateMeta
    , cMeta :: CertificateMeta
    } deriving (Show, Eq, Ord, Generic)

instance Buildable Certificate where
    build Certificate {..} =
        "{ id = "+|cId|+", meta = "+|cMeta|+" }"

-- | Datatype which is used for encoding a full certificate ID.
data CertificateName = CertificateName
    { cnEducatorId    :: Text
    , cnCertificateId :: Hash CertificateMeta
    } deriving (Show, Eq, Ord, Generic)

instance Buildable CertificateName where
    build (CertificateName eId cId) =
        "certificate { educator-id = "+|eId|+", hash = "+|build cId|+"}"

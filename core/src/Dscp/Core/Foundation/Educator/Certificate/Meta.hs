
module Dscp.Core.Foundation.Educator.Certificate.Meta where

import Data.Time.Calendar (Day (..))

import Dscp.Core.Foundation.Educator.ItemDesc
import Dscp.Core.Foundation.Educator.Certificate.EducationForm
import Dscp.Util

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
    } deriving (Show, Eq, Generic)

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


{-# LANGUAGE StrictData #-}

-- | Types specific to educator API.

module Dscp.Educator.Web.Educator.Types
    (
      -- * Requests
      NewStudent (..)
    , NewCourse (..)
    , NewGrade (..)
    , NewAssignment (..)
    , NewStudentCourse (..)
    , NewStudentAssignment (..)

      -- * Responses
    , Counted (..)
    , mkCountedList
    , EducatorInfo (..)
    , CourseEducatorInfo (..)
    , AssignmentEducatorInfo (..)
    , SubmissionEducatorInfo (..)
    , Certificate (..)
    , CertificateGrade (..)
    , CertificateFullInfo (..)
    , eaDocumentType

      -- * Conversions
    , educatorLiftAssignment
    , educatorLiftSubmission
    , requestToAssignment
    , educatorAssignmentInfoFromRow
    , educatorSubmissionInfoFromRow
    ) where

import Control.Lens (from)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Fmt (build, listF, (+|), (|+))
import Servant.Util (type (?:), ForResponseLog (..), SortingParamTypesOf, buildListForResponse)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQL.Util
import Dscp.Educator.DB.Schema
import Dscp.Educator.Web.Types
import Dscp.Witness.Web.Types

data NewStudent = NewStudent
    { nsAddr :: Student
    } deriving (Show, Eq, Generic)

data NewCourse = NewCourse
    { ncId       :: Maybe Course
    , ncDesc     :: ItemDesc
    , ncSubjects :: [Subject]
    } deriving (Show, Eq, Generic)

data NewGrade = NewGrade
    { ngSubmissionHash :: (Hash Submission)
    , ngGrade          :: Grade
    }

data NewAssignment = NewAssignment
    { naCourseId     :: Course
    , naContentsHash :: (Hash Raw)
    , naIsFinal      :: IsFinal
    , naDesc         :: ItemDesc
    } deriving (Show, Eq, Generic)

data NewStudentCourse = NewStudentCourse
    { nscCourseId :: Course
    } deriving (Show, Eq, Generic)

data NewStudentAssignment = NewStudentAssignment
    { nsaAssignmentHash :: Hash Assignment
    }

data EducatorInfo = EducatorInfo
    { eiAddress  :: Address
    , eiBalances :: BlocksOrMempool Coin
    } deriving (Show, Eq, Ord, Generic)

data CourseEducatorInfo = CourseEducatorInfo
    { ciId       :: Course
    , ciDesc     :: ItemDesc
    , ciSubjects :: [Subject]
    } deriving (Show, Eq, Ord, Generic)

data AssignmentEducatorInfo = AssignmentEducatorInfo
    { aiHash         :: (Hash Assignment)
    , aiCourseId     :: Course
    , aiContentsHash :: (Hash Raw)
    , aiIsFinal      :: IsFinal
    , aiDesc         :: ItemDesc
    } deriving (Show, Eq, Generic)

data SubmissionEducatorInfo = SubmissionEducatorInfo
    { siHash           :: (Hash Submission)
    , siContentsHash   :: (Hash Raw)
    , siAssignmentHash :: (Hash Assignment)
    , siGrade          :: (Maybe GradeInfo)
    , siWitness        :: SubmissionWitness
    } deriving (Show, Eq, Generic)

eaDocumentType :: AssignmentEducatorInfo -> DocumentType
eaDocumentType = documentType . aiContentsHash

-- | Datatype which combines certificate meta with its ID.
data Certificate = Certificate
    { cId   :: Hash CertificateMeta
    , cMeta :: CertificateMeta
    } deriving (Show, Eq, Generic)

-- | Datatype which contains information about the grade which
-- gets included into the certificate.
data CertificateGrade = CertificateGrade
    { cgSubject :: ItemDesc
    , cgLang    :: Language
    , cgHours   :: Int
    , cgCredits :: Int
    , cgGrade   :: Grade
    } deriving (Show, Eq, Generic)

-- | Datatype which contains all the info about certificate. This
-- datatype represents a request body for 'AddCertificate' endpoint.
data CertificateFullInfo = CertificateFullInfo
    { cfiMeta   :: CertificateMeta
    , cfiGrades :: [CertificateGrade]
    } deriving (Show, Eq, Generic)

data Language = EN | RU
    deriving (Show, Eq, Generic)

-- | Special wrapper for list which includes its length
data Counted a = Counted
    { cCount :: Int
    , cItems :: Maybe [a]
    } deriving (Show, Eq, Generic)

-- | Makes a @Counted@ from a list, omitting the list itself if
-- 'onlyCount' flag is set
mkCountedList :: Bool -> [a] -> Counted a
mkCountedList onlyCount ls =
    Counted (length ls) (if onlyCount then Nothing else Just ls)

---------------------------------------------------------------------------
-- Sorting
---------------------------------------------------------------------------

type instance SortingParamTypesOf Certificate =
    ["createdAt" ?: Timestamp, "student" ?: ItemDesc]

---------------------------------------------------------------------------
-- Simple conversions
---------------------------------------------------------------------------

educatorLiftAssignment :: Assignment -> AssignmentEducatorInfo
educatorLiftAssignment a =
    AssignmentEducatorInfo
    { aiHash = hash a
    , aiCourseId = _aCourseId a
    , aiContentsHash = _aContentsHash a
    , aiIsFinal = _aType a ^. assignmentTypeRaw
    , aiDesc = _aDesc a
    }

educatorLiftSubmission :: SignedSubmission -> Maybe GradeInfo -> SubmissionEducatorInfo
educatorLiftSubmission ss siGrade =
    SubmissionEducatorInfo
    { siHash = hash s
    , siContentsHash = _sContentsHash s
    , siAssignmentHash = _sAssignmentHash s
    , siWitness = _ssWitness ss
    , siGrade
    }
  where
    s = _ssSubmission ss

requestToAssignment :: NewAssignment -> Assignment
requestToAssignment NewAssignment{..} =
    Assignment
    { _aCourseId = naCourseId
    , _aContentsHash = naContentsHash
    , _aType = naIsFinal ^. from assignmentTypeRaw
    , _aDesc = naDesc
    }

educatorAssignmentInfoFromRow :: AssignmentRow -> AssignmentEducatorInfo
educatorAssignmentInfoFromRow AssignmentRow{..} =
    AssignmentEducatorInfo
    { aiHash = arHash
    , aiContentsHash = arContentsHash
    , aiDesc = arDesc
    , aiCourseId = case arCourse of CourseRowId cId -> cId
    , aiIsFinal = arType ^. assignmentTypeRaw
    }

educatorSubmissionInfoFromRow :: (SubmissionRow, Maybe TransactionRow) -> SubmissionEducatorInfo
educatorSubmissionInfoFromRow (SubmissionRow{..}, tx) =
    SubmissionEducatorInfo
    { siHash = srHash
    , siContentsHash = srContentsHash
    , siAssignmentHash = unpackPk srAssignment
    , siGrade = fmap gradeInfoFromRow tx
    , siWitness = srSignature
    }

---------------------------------------------------------------------------
-- Buildable instances
---------------------------------------------------------------------------

instance Buildable (NewStudent) where
    build (NewStudent{..}) =
      "{ address = " +| nsAddr |+
      " }"

instance Buildable (NewCourse) where
    build (NewCourse{..}) =
      "{ course id = " +| ncId |+
      ", description = " +| ncDesc |+
      ", subjects = " +| listF ncSubjects |+
      " }"

instance Buildable (NewGrade) where
    build (NewGrade{..}) =
      "{ submission hash = " +| ngSubmissionHash |+
      ", grade = " +| ngGrade |+
      " }"

instance Buildable (NewAssignment) where
    build (NewAssignment{..}) =
      "{ course id = " +| naCourseId |+
      ", is final = " +| naIsFinal |+
      ", description = " +| naDesc |+
      " }"

instance Buildable (NewStudentCourse) where
    build (NewStudentCourse{..}) =
      "{ course id = " +| nscCourseId |+
      " }"

instance Buildable NewStudentAssignment where
    build (NewStudentAssignment{..}) =
      "{ hash = " +| nsaAssignmentHash |+
      " }"

instance Buildable (CourseEducatorInfo) where
    build (CourseEducatorInfo{..}) =
      "{ course id = " +| ciId |+
      ", description = " +| ciDesc |+
      ", subjects = " +| listF ciSubjects |+
      " }"

instance Buildable (AssignmentEducatorInfo) where
    build (AssignmentEducatorInfo{..}) =
      "{ hash = " +| aiHash |+
      ", course id = " +| aiCourseId |+
      ", description = " +| aiDesc |+
      " }"

instance Buildable (SubmissionEducatorInfo) where
    build (SubmissionEducatorInfo{..}) =
      "{ hash = " +| siHash |+
      ", content hash = " +| siContentsHash |+
      ", assignment hash = " +| siAssignmentHash |+
      " }"

instance Buildable Certificate where
    build Certificate {..} =
        "{ id = "+|cId|+", meta = "+|cMeta|+" }"

instance Buildable CertificateGrade where
    build CertificateGrade {..} =
        "{ subject = "+|cgSubject|+
        ", hours = "+|cgHours|+
        ", credits = "+|cgCredits|+
        ", grade = "+|cgGrade|+" }"

instance Buildable CertificateFullInfo where
    build CertificateFullInfo {..} =
        "{ meta = "+|cfiMeta|+
        ", grades = "+|listF cfiGrades|+" }"

instance Buildable (ForResponseLog EducatorInfo) where
    build (ForResponseLog EducatorInfo{..})=
      "{ address = " +| eiAddress |+
      ", balances = " +| eiBalances |+
      " }"

instance Buildable (ForResponseLog CourseEducatorInfo) where
    build (ForResponseLog CourseEducatorInfo{..}) =
      "{ course id = " +| ciId |+
      " }"

instance Buildable (ForResponseLog AssignmentEducatorInfo) where
    build (ForResponseLog AssignmentEducatorInfo{..}) =
      "{ hash = " +| aiHash |+
      " }"

instance Buildable (ForResponseLog SubmissionEducatorInfo) where
    build (ForResponseLog SubmissionEducatorInfo{..})=
      "{ hash = " +| siHash |+
      " }"

instance Buildable (ForResponseLog Certificate) where
    build (ForResponseLog Certificate{..}) = "{ id = "+|cId|+" }"

instance Buildable (ForResponseLog CertificateGrade) where
    build (ForResponseLog CertificateGrade {..}) =
        "{ subject = "+|cgSubject|+
        ", grade = "+|cgGrade|+" }"

instance Buildable (ForResponseLog CertificateFullInfo) where
    build (ForResponseLog CertificateFullInfo {..}) =
        "{ meta = "+|cfiMeta|+
        ", grades = "+|buildListForResponse (take 4) (ForResponseLog cfiGrades)|+" }"

-- Instance for PDF contents
instance Buildable (ForResponseLog LByteString) where
    build _ = "<binary data>"

instance Buildable (ForResponseLog [CourseEducatorInfo]) where
    build = buildListForResponse (take 6)

instance Buildable (ForResponseLog [AssignmentEducatorInfo]) where
    build = buildListForResponse (take 4)

instance Buildable (ForResponseLog [SubmissionEducatorInfo]) where
    build = buildListForResponse (take 4)

instance Buildable (ForResponseLog [Certificate]) where
    build = buildListForResponse (take 4)

instance Buildable (ForResponseLog [a]) =>
         Buildable (ForResponseLog (Counted a)) where
    build (ForResponseLog (Counted n mbLs)) =
        "Counted { n = "+|n|+", items = "+|mbItems mbLs|+" }"
      where
        mbItems Nothing   = "omitted"
        mbItems (Just ls) = build (ForResponseLog ls)

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

instance ToJSON Language where
    toJSON EN = String "en"
    toJSON RU = String "ru"
instance FromJSON Language where
    parseJSON = withText "Language" $ \case
        "en" -> pure EN
        "ru" -> pure RU
        other -> fail $ "invalid constructor: " ++ toString other

deriveJSON defaultOptions ''NewStudent
deriveJSON defaultOptions ''NewCourse
deriveJSON defaultOptions ''NewGrade
deriveJSON defaultOptions ''NewAssignment
deriveJSON defaultOptions ''NewStudentCourse
deriveJSON defaultOptions ''NewStudentAssignment
deriveJSON defaultOptions ''EducatorInfo
deriveJSON defaultOptions ''CourseEducatorInfo
deriveJSON defaultOptions ''AssignmentEducatorInfo
deriveJSON defaultOptions ''SubmissionEducatorInfo
deriveJSON defaultOptions ''Certificate
deriveJSON defaultOptions ''CertificateGrade
deriveJSON defaultOptions ''CertificateFullInfo
deriveJSON defaultOptions ''Counted

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
    , mkCertificate
    , eaDocumentType

      -- * Conversions
    , educatorLiftAssignment
    , educatorLiftSubmission
    , requestToAssignment
    , educatorAssignmentInfoFromRow
    , educatorSubmissionInfoFromRow
    ) where

import Control.Lens (at, from, (%=), (.=), (?=), _Just)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Char (toLower)
import Data.Swagger (ToSchema (..))
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Swagger.Internal.Schema as S
import Data.Time.Calendar (Day)
import Fmt (build, listF, (+|), (|+))
import Pdf.Scanner (PDFBody (..))
import Servant.Util (type (?:), ForResponseLog (..), SortingParamTypesOf, buildListForResponse)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQL.Util
import Dscp.Educator.DB.Schema
import Dscp.Educator.Web.Types
import Dscp.Util
import Dscp.Web.Swagger
import Dscp.Witness.Web.Types

data NewStudent = NewStudent
    { nsAddr :: Student
    } deriving (Show, Eq, Generic)

data NewCourse = NewCourse
    { ncDesc     :: ItemDesc
    , ncSubjects :: [Subject]
    } deriving (Show, Eq, Generic)

data NewGrade = NewGrade
    { ngSubmissionHash :: (Hash Submission)
    , ngGrade          :: Grade
    } deriving (Generic)

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
    } deriving (Generic)

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

eaDocumentType :: AssignmentEducatorInfo -> DocumentType Assignment
eaDocumentType = documentType . aiContentsHash

-- | Makes a 'Certificate' from 'CertificateMeta'.
mkCertificate :: CertificateMeta -> Certificate
mkCertificate meta = Certificate (hash meta) meta

-- | Special wrapper for list which includes its length
data Counted a = Counted
    { cCount :: Int
    , cItems :: Maybe [a]
    } deriving (Show, Eq, Generic)

-- | Makes a 'Counted' from a list, omitting the list itself if
-- @onlyCount@ flag is set
mkCountedList :: Bool -> [a] -> Counted a
mkCountedList onlyCount ls =
    Counted (length ls) (if onlyCount then Nothing else Just ls)

---------------------------------------------------------------------------
-- Sorting
---------------------------------------------------------------------------

type instance SortingParamTypesOf Certificate =
    ["createdAt" ?: Day, "studentName" ?: ItemDesc]

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

educatorSubmissionInfoFromRow :: (SubmissionRow, Maybe GradeRow) -> SubmissionEducatorInfo
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
      "{ description = " +| ncDesc |+
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
        ", grades = "+|buildListForResponse (take 4) (ForResponseLog $ toList cfiGrades)|+" }"

instance Buildable (ForResponseLog PDFBody) where
    build _ = "<pdf>"

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

deriveJSON defaultOptions ''AssignmentEducatorInfo
deriveJSON defaultOptions ''Certificate
deriveJSON defaultOptions ''Counted
deriveJSON defaultOptions ''CourseEducatorInfo
deriveJSON defaultOptions ''EducatorInfo
deriveJSON defaultOptions ''NewAssignment
deriveJSON defaultOptions ''NewCourse
deriveJSON defaultOptions ''NewGrade
deriveJSON defaultOptions ''NewStudent
deriveJSON defaultOptions ''NewStudentAssignment
deriveJSON defaultOptions ''NewStudentCourse
deriveJSON defaultOptions ''SubmissionEducatorInfo

---------------------------------------------------------------------------
-- Swagger instances
---------------------------------------------------------------------------

type instance ParamDescription Coin =
    "Coins amount."
type instance ParamDescription PDFBody =
    "Content of a PDF file."
type instance ParamDescription IsGraded =
    "Return only submissions with/without grade."

type instance QueryFlagDescription "autoAssign" =
    "Automatically subscribe all students attending related course to new \
    \assignment."

instance ToSchema a => ToSchema (Counted a) where
    declareNamedSchema _ = do
        S.declare $ mempty &: do
            at countName ?= countSchema
            whenJust (S.schemaName (Proxy @a)) $ \name ->
                at name ?= S.toSchema (Proxy @a)

        S.plain $ mempty &: do
            S.type_ .= S.SwaggerObject
            S.required .= ["count"]
            S.properties .= mempty &: do
                at "count" ?= S.Ref (S.Reference countName)
                at "items" ?= S.toSchemaRef (Proxy @[a])
      where
        countName = "Count"
        countSchema = mempty &: do
            S.type_ .= S.SwaggerInteger
            S.format ?= "int32"
            S.description ?= "Count of items returned"

instance ToSchema Coin where
    declareNamedSchema p =
        S.plain $ mempty &: do
            S.type_ .= S.SwaggerInteger
            S.format ?= "int32"
            setParamDescription p

instance ToSchema a => ToSchema (BlocksOrMempool a) where
    declareNamedSchema _ =
        S.plain $ mempty &: do
            S.type_ .= S.SwaggerObject
            S.required .= ["confirmed", "total"]
            S.properties .= mempty &: do
                at "confirmed" ?= S.toSchemaRef (Proxy @a) &: do
                    _Inline . S.description . _Just %= ("From blockchain-only view. " <>)

                at "total" ?= S.toSchemaRef (Proxy @a) &: do
                    _Inline . S.description . _Just %=
                        ("From view considering both chain and pending changes. " <>)

instance ToSchema Language where
    declareNamedSchema =
        S.genericDeclareNamedSchema
        dscpSchemaOptions{ S.constructorTagModifier = map toLower }

instance ToSchema PDFBody where
    declareNamedSchema p =
        declareSimpleSchema "PDFBody" $ S.binarySchema &: do
            setParamDescription p

instance ToSchema Certificate where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema EducationForm where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema GradingScale where
    declareNamedSchema p =
        inDeclaredSchema (gDeclareNamedSchema p) $
            setDocEnumDescription @GradingScale

instance ToSchema CertificateGrade where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema CertificateMeta where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema CertificateFullInfo where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema NewCourse where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema NewStudent where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema NewAssignment where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema NewStudentCourse where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema NewStudentAssignment where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema NewGrade where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema EducatorInfo where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema CourseEducatorInfo where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema AssignmentEducatorInfo where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema SubmissionEducatorInfo where
    declareNamedSchema = gDeclareNamedSchema

instance EnumHasDescription GradingScale where
    enumDocDescription p = enumCaseDocDesc p $ \case
        RusDiff ->
            "Russian differentiated: 5 = 100, 4 = 80, 3 = 60, 2 = 40"
        RusNonDiff ->
            "Russian non-differentiated: \"zachot\" = 100, \"nezachot\" = 0"

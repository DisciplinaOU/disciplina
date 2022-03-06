{-# LANGUAGE GADTs         #-}
{-# LANGUAGE StrictData    #-}
{-# LANGUAGE TypeOperators #-}

-- | Common datatypes for educator and student HTTP APIs

module Dscp.Educator.Web.Types
       (
         MonadEducatorWebQuery
       , MonadEducatorWeb

         -- * Flags
       , IsEnrolled (..)
       , IsGraded (..)
       , IsFinal (..)
       , _IsFinal
       , HasProof (..)

         -- * Responses
       , StudentInfo (..)
       , GradeInfo (..)
       , BlkProofInfo (..)

         -- * Conversions
       , assignmentTypeRaw
       , gradeInfoFromRow
       , certificateFromRow
       ) where

import Universum

import Control.Lens (Iso', from, iso, makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Swagger (ToParamSchema (..), ToSchema (..))
import Fmt (Buildable (..), (+|), (+||), (|+), (||+))
import Loot.Base.HasLens (HasCtx)
import Loot.Log (LoggingIO, ModifyLogName, MonadLogging)
import qualified Pdf.FromLatex as Pdf
-- import Servant (FromHttpApiData, ToHttpApiData)
import Servant.Util (ForResponseLog (..), buildForResponse, buildListForResponse)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQL
import Dscp.Educator.DB
import Dscp.Educator.Launcher.Mode
import Dscp.Educator.Launcher.Resource (CertificateIssuerResource)
import Dscp.Educator.Resource
import Dscp.Resource.AppDir
import Dscp.Util.Aeson
import Dscp.Web.Swagger

type MonadEducatorWebQuery m =
    ( MonadIO m
    , MonadCatch m
    , MonadLogging m
    , ModifyLogName m
    )

type MonadEducatorWeb ctx m =
    ( BasicWorkMode m
    , HasCtx ctx m
        [ LoggingIO
        , AppDir
        , SQL
        , KeyResources EducatorNode
        , Language
        , Pdf.LatexPath
        , Pdf.ResourcePath
        , Pdf.DownloadBaseUrl
        , CertificateIssuerResource
        ]
    )

---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------

-- | Whether student is enrolled into a course.
newtype IsEnrolled = IsEnrolled
    { unIsEnrolled :: Bool
    } deriving (Eq, Show, Generic)

-- | Whether assignment is final in course.
newtype IsFinal = IsFinal
    { unIsFinal :: Bool
    } deriving (Eq, Show, Generic)

makePrisms ''IsFinal

-- | Whether submission is graded.
newtype IsGraded = IsGraded
    { unIsGraded :: Bool
    } deriving (Eq, Show, Generic)

-- | Whether transaction has been published into public chain.
newtype HasProof = HasProof { unHasProof :: Bool }
    deriving (Eq, Show)

data BlkProofInfo = BlkProofInfo
    { bpiBlockHash       :: PrivateHeaderHash
    , bpiMtreeSerialized :: EncodeSerialised Base64Encoded (EmptyMerkleProof PrivateTx)
    , bpiTxs             :: [PrivateTx]
    } deriving (Show, Eq, Generic)

---------------------------------------------------------------------------
-- Buildable instances
---------------------------------------------------------------------------

instance Buildable (IsEnrolled) where
    build (IsEnrolled{..}) =
      "{ is enrolled = " +| unIsEnrolled |+
      " }"

instance Buildable (IsFinal) where
    build (IsFinal{..}) =
      "{ is final = " +| unIsFinal |+
      " }"

instance Buildable (IsGraded) where
    build (IsGraded{..}) =
      "{ is enrolled = " +| unIsGraded |+
      " }"

instance Buildable (BlkProofInfo) where
    build (BlkProofInfo{..}) =
      "{ tree root hash = " +||
          fmap (reconstructRoot . unEmptyProof) bpiMtreeSerialized ||+
      ", transactons num = " +| length bpiTxs |+
      "} "

instance Buildable (ForResponseLog StudentInfo) where
    build = buildForResponse

instance Buildable (ForResponseLog GradeInfo) where
    build (ForResponseLog GradeInfo{..}) =
      "{ submission hash = " +| giSubmissionHash |+
      ", grade = " +| giGrade |+
      " }"

instance Buildable (ForResponseLog BlkProofInfo) where
    build (ForResponseLog BlkProofInfo{..}) =
      "{ tree root hash = " +||
          fmap (reconstructRoot . unEmptyProof) bpiMtreeSerialized ||+
      "} "

instance Buildable (ForResponseLog Course) where
    build = buildForResponse

instance Buildable (ForResponseLog [GradeInfo]) where
    build = buildListForResponse (take 4)

instance Buildable (ForResponseLog [StudentInfo]) where
    build = buildListForResponse (take 8)

instance Buildable (ForResponseLog [BlkProofInfo]) where
    build = buildListForResponse (take 8)

---------------------------------------------------------------------------
-- Simple conversions
---------------------------------------------------------------------------

assignmentTypeRaw :: Iso' AssignmentType IsFinal
assignmentTypeRaw = iso forth back . from _IsFinal
  where
    back = \case
        False -> Regular
        True  -> CourseFinal
    forth = \case
        Regular     -> False
        CourseFinal -> True

gradeInfoFromRow :: TransactionRow -> GradeInfo
gradeInfoFromRow TransactionRow{..} =
    GradeInfo
    { giSubmissionHash = unpackPk trSubmission
    , giGrade = trGrade
    , giTimestamp = trCreationTime
    , giHasProof = trIdx /= TxInMempool
    }

certificateFromRow :: (Hash CertificateMeta, PgJSONB CertificateMeta) -> Certificate
certificateFromRow (cId, meta) =
    Certificate
    { cId
    , cMeta = case meta of PgJSONB m -> m
    }

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriving instance ToJSON IsEnrolled
deriving instance FromJSON IsEnrolled

deriving instance ToJSON IsFinal
deriving instance FromJSON IsFinal

deriving instance ToJSON IsGraded
deriving instance FromJSON IsGraded

deriving instance ToJSON HasProof
deriving instance FromJSON HasProof

deriveJSON defaultOptions ''GradeInfo
deriveJSON defaultOptions ''StudentInfo
deriveJSON defaultOptions ''BlkProofInfo

---------------------------------------------------------------------------
-- To/FromHttpApiData instances
---------------------------------------------------------------------------

deriving instance FromHttpApiData IsEnrolled
deriving instance FromHttpApiData IsFinal
deriving instance FromHttpApiData IsGraded

deriving instance ToHttpApiData IsEnrolled
deriving instance ToHttpApiData IsFinal
deriving instance ToHttpApiData IsGraded

---------------------------------------------------------------------------
-- Swagger instances
---------------------------------------------------------------------------

type instance ParamDescription IsEnrolled =
    "`true` when the student is currently enrolled to the course, \
    \`false` when the course is yet only available for the student."
type instance ParamDescription IsFinal =
    "Whether assignment is final/non-final."


instance ToParamSchema IsEnrolled where
    toParamSchema = gToParamSchema

instance ToParamSchema IsFinal where
    toParamSchema = gToParamSchema

instance ToParamSchema IsGraded where
    toParamSchema = gToParamSchema


instance ToSchema IsFinal where
    declareNamedSchema = newtypeDeclareNamedSchema @Bool

instance ToSchema StudentInfo where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema GradeInfo where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema BlkProofInfo where
    declareNamedSchema = gDeclareNamedSchema

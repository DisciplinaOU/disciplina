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
       ) where

import Control.Lens (Iso', from, iso, makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (UTCTime)
import Fmt (build, (+|), (+||), (|+), (||+))
import Loot.Base.HasLens (HasCtx)
import Loot.Log (ModifyLogName, MonadLogging)
import Servant (FromHttpApiData (..), ToHttpApiData)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQLite
import Dscp.Educator.DB
import Dscp.Educator.Launcher.Marker
import Dscp.Resource.Keys
import Dscp.Util.Aeson
import Dscp.Util.Servant
import Dscp.Witness.Launcher.Context

type MonadEducatorWebQuery m =
    ( MonadIO m
    , MonadCatch m
    , MonadLogging m
    , ModifyLogName m
    )

type MonadEducatorWeb ctx m =
    ( WitnessWorkMode ctx m
    , HasCtx ctx m '[SQLiteDB, KeyResources EducatorNode]
    )

---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------

-- | Whether student is enrolled into a course.
newtype IsEnrolled = IsEnrolled
    { unIsEnrolled :: Bool
    } deriving (Eq, Show)

-- | Whether assignment is final in course.
newtype IsFinal = IsFinal
    { unIsFinal :: Bool
    } deriving (Eq, Show)

makePrisms ''IsFinal

-- | Whether submission is graded.
newtype IsGraded = IsGraded
    { unIsGraded :: Bool
    } deriving (Eq, Show)

-- | Whether transaction has been published into public chain.
newtype HasProof = HasProof { unHasProof :: Bool }
    deriving (Eq, Show)

data StudentInfo = StudentInfo
    { siAddr :: Student
    } deriving (Show, Eq, Ord, Generic)

data GradeInfo = GradeInfo
    { giSubmissionHash :: (Hash Submission)
    , giGrade          :: Grade
    , giTimestamp      :: UTCTime
    , giHasProof       :: Bool
    } deriving (Show, Eq, Ord, Generic)

data BlkProofInfo = BlkProofInfo
    { bpiBlockHash       :: PrivateHeaderHash
    , bpiMtreeSerialized :: (EncodeSerialised Base64Encoded (EmptyMerkleProof PrivateTx))
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

instance Buildable (StudentInfo) where
    build (StudentInfo{..}) =
      "{ address = " +| siAddr |+
      " }"

instance Buildable (GradeInfo) where
    build (GradeInfo{..}) =
      "{ submission hash = " +| giSubmissionHash |+
      ", grade = " +| giGrade |+
      ", timestamp = " +| giTimestamp |+
      ", has proof = " +| giHasProof |+
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
    build = buildShortResponseList

instance Buildable (ForResponseLog [StudentInfo]) where
    build = buildLongResponseList

instance Buildable (ForResponseLog [BlkProofInfo]) where
    build = buildLongResponseList

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

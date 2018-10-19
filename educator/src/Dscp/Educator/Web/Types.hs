{-# LANGUAGE GADTs      #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}

-- | Common datatypes for educator and student HTTP APIs

module Dscp.Educator.Web.Types
       (
         MonadEducatorWebQuery
       , MonadEducatorWeb

         -- * Multi API types
       , ApiTag (..)
       , ApiCase (..)
       , ResponseCase
       , DistinctTag

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
       ) where

import Control.Lens (Iso', from, iso, makePrisms)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple (FromRow (..), field)
import Servant (FromHttpApiData (..))
import Data.Singletons.Bool (SBoolI)
import UnliftIO (MonadUnliftIO)
import Loot.Log (MonadLogging)
import Loot.Base.HasLens (HasCtx)
import Fmt (build, (+|), (|+), (+||), (||+))

import Dscp.DB.SQLite.Types
import Dscp.Util.Type (type (==))
import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQLite.Instances ()
import Dscp.Util.Aeson (CustomEncoding, HexEncoded)
import Dscp.Util.Servant (ForResponseLog (..), buildForResponse,
                          buildShortResponseList, buildLongResponseList)

type MonadEducatorWebQuery m =
    ( MonadIO m
    , MonadCatch m
    , MonadLogging m
    )

type MonadEducatorWeb ctx m =
    ( MonadUnliftIO m
    , MonadCatch m
    , MonadLogging m
    , HasCtx ctx m '[SQLiteDB]
    )

---------------------------------------------------------------------------
-- API's distinction
---------------------------------------------------------------------------

-- | Tag indicating an API.
data ApiTag = StudentTag | EducatorTag

-- | Various functions are going to serve student and educator cases
-- simultaniously thus returning slightly different types.
type family ResponseCase (apiTag :: ApiTag) baseType

-- | Defines which case (student or educator) a function should handle,
-- returning appropriate 'ResponseCase'.
data ApiCase a where
    StudentCase  :: ApiCase 'StudentTag
    EducatorCase :: ApiCase 'EducatorTag

-- | Declares that we can compare this tag against other 'ApiTag's.
type DistinctTag tag =
    ( SBoolI (tag == 'StudentTag)
    , SBoolI (tag == 'EducatorTag)
    )

---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------

-- | Whether student is enrolled into a course.
newtype IsEnrolled = IsEnrolled { unIsEnrolled :: Bool }
    deriving (Eq, Show)

-- | Whether assignment is final in course.
newtype IsFinal = IsFinal { unIsFinal :: Bool }
    deriving (Eq, Show)

makePrisms ''IsFinal

-- | Whether submission is graded.
newtype IsGraded = IsGraded { unIsGraded :: Bool }
    deriving (Eq, Show)

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
    { bpiMtreeSerialized :: (CustomEncoding HexEncoded (MerkleProof PrivateTx))
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
          fmap getMerkleProofRoot bpiMtreeSerialized ||+
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
          fmap getMerkleProofRoot bpiMtreeSerialized ||+
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

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''IsEnrolled
deriveJSON defaultOptions ''IsFinal
deriveJSON defaultOptions ''IsGraded
deriveJSON defaultOptions ''HasProof
deriveJSON defaultOptions ''GradeInfo
deriveJSON defaultOptions ''StudentInfo
deriveJSON defaultOptions ''BlkProofInfo

---------------------------------------------------------------------------
-- FromHttpApiData instances
---------------------------------------------------------------------------

deriving instance FromHttpApiData IsEnrolled
deriving instance FromHttpApiData IsFinal
deriving instance FromHttpApiData IsGraded

---------------------------------------------------------------------------
-- SQLite instances
---------------------------------------------------------------------------

instance FromRow GradeInfo where
    fromRow = GradeInfo <$> field <*> field <*> field
                        <*> ((/= TxInMempool) <$> field)
instance FromRow (Maybe GradeInfo) where
    fromRow =
        liftM4 GradeInfo <$> field <*> field <*> field
                         <*> (fmap (/= TxInMempool) <$> field)

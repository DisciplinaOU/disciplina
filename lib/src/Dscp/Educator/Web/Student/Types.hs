
-- | Datatypes for Student HTTP API

module Dscp.Educator.Web.Student.Types
       ( Student
       , IsFinal (..)
       , IsEnrolled (..)
       , HasProof (..)
       , Course (..)
       , Assignment (..)
       , Submission (..)
       , Grade (..)
       , BlkProof (..)
       , ErrResponse (..)

       , _IsFinal
       , assignmentTypeRaw
       , liftAssignment
       , liftSubmission
       , aDocumentType
       ) where

import Control.Lens (Iso', iso, makePrisms)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (UTCTime)
import Servant (FromHttpApiData (..))

import Dscp.Core.Address (addrFromText)
import Dscp.Core.Aeson ()
import qualified Dscp.Core.Types as Core
import Dscp.Crypto (hash)
import Dscp.Crypto (Hash, Raw)
import Dscp.Educator.Txs (PrivateTx (..))
import Dscp.Educator.Web.Student.Error (APIError)
import Dscp.Util (fromBase64)
import Dscp.Util.Aeson (AsByteString, Base64Encoded)

type Student = Core.Student

-- | Whether assignment is final in course.
newtype IsFinal = IsFinal { unIsFinal :: Bool }
    deriving (Eq, Show)

makePrisms ''IsFinal

-- | Whether student is enrolled into a course.
newtype IsEnrolled = IsEnrolled { unIsEnrolled :: Bool }
    deriving (Eq, Show)

-- | Whether transaction has been published into public chain.
newtype HasProof = HasProof { unHasProof :: Bool }
    deriving (Eq, Show)

data Course = Course
    { cId         :: !Core.Course
    , cDesc       :: !Text
    , cSubjects   :: ![Core.Subject]
    , cIsEnrolled :: !Bool
    } deriving (Show, Eq, Generic)

data Assignment = Assignment
    { aHash           :: !(Hash Core.Assignment)
    , aCourseId       :: !Core.Course
    , aContentsHash   :: !(Hash Raw)
    , aIsFinal        :: !Bool
    , aDesc           :: !Text
    , aLastSubmission :: !(Maybe Submission)
    } deriving (Show, Eq, Generic)

data Submission = Submission
    { sHash           :: !(Hash Core.Submission)
    , sContentsHash   :: !(Hash Raw)
    , sAssignmentHash :: !(Hash Core.Assignment)
    , sGrade          :: !(Maybe Grade)
    } deriving (Show, Eq, Generic)

data Grade = Grade
    { gGrade     :: !Core.Grade
    , gTimestamp :: !UTCTime
    , gHasProof  :: !Bool
    } deriving (Show, Eq, Generic)

data BlkProof = BlkProof
    { bpMtreeSerialized :: !(AsByteString Base64Encoded ByteString)
    , bpTxs             :: ![PrivateTx]
    } deriving (Show, Eq, Generic)

data ErrResponse = ErrResponse
    { erError :: !APIError
    } deriving (Show, Eq, Generic)

---------------------------------------------------------------------------
-- Simple functions
---------------------------------------------------------------------------

assignmentTypeRaw :: Iso' Core.AssignmentType IsFinal
assignmentTypeRaw = iso forth back
  where
    back = \case
        IsFinal False -> Core.Regular
        IsFinal True  -> Core.CourseFinal
    forth = \case
        Core.Regular     -> IsFinal False
        Core.CourseFinal -> IsFinal True

liftAssignment :: Core.Assignment -> Maybe Submission -> Assignment
liftAssignment a lastSubmission =
    Assignment
    { aHash = hash a
    , aCourseId = Core._aCourseId a
    , aContentsHash = Core._aContentsHash a
    , aIsFinal = Core._aType a ^. assignmentTypeRaw . _IsFinal
    , aDesc = Core._aDesc a
    , aLastSubmission = lastSubmission
    }

liftSubmission :: Core.Submission -> Maybe Grade -> Submission
liftSubmission s sGrade =
    Submission
    { sHash = hash s
    , sContentsHash = Core._sContentsHash s
    , sAssignmentHash = hash (Core._sAssignment s)
    , ..
    }

aDocumentType :: Assignment -> Core.DocumentType
aDocumentType = Core.documentType . aContentsHash

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''PrivateTx

deriveJSON defaultOptions ''IsFinal
deriveJSON defaultOptions ''IsEnrolled
deriveJSON defaultOptions ''HasProof
deriveJSON defaultOptions ''Course
deriveJSON defaultOptions ''Assignment
deriveJSON defaultOptions ''Submission
deriveJSON defaultOptions ''Grade
deriveJSON defaultOptions ''BlkProof
deriveJSON defaultOptions ''ErrResponse

---------------------------------------------------------------------------
-- FromHttpApiData instances
---------------------------------------------------------------------------

deriving instance FromHttpApiData Core.Course
deriving instance FromHttpApiData Core.Subject
deriving instance FromHttpApiData Core.Grade
deriving instance FromHttpApiData IsEnrolled
deriving instance FromHttpApiData IsFinal

instance FromHttpApiData Core.DocumentType where
    parseQueryParam "offline" = Right Core.Offline
    parseQueryParam "online"  = Right Core.Online
    parseQueryParam other     = Left $ "invalid document type: " <> other

instance FromHttpApiData Core.Address where
    parseQueryParam = addrFromText

instance FromHttpApiData (Hash a) where
    parseQueryParam = first toText . fromBase64

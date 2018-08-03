-- | Datatypes for Student HTTP API

module Dscp.Educator.Web.Types
       (
         -- * Flags
         IsFinal (..)
       , _IsFinal
       , IsEnrolled (..)
       , HasProof (..)

         -- * Responses
       , CourseInfo (..)
       , AssignmentInfo (..)
       , SubmissionInfo (..)
       , GradeInfo (..)
       , BlkProofInfo (..)

         -- * Requests
       , NewSubmission (..)
       , nsOwner
       , NewCourse (..)
       , NewGrade (..)
       , NewAssignment (..)

         -- * Conversions
       , assignmentTypeRaw
       , liftAssignment
       , liftSubmission
       , signedSubmissionToRequest
       , aiDocumentType
       ) where

import Control.Lens (Iso', iso, makePrisms)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (UTCTime)
import Servant (FromHttpApiData (..))

import Dscp.Core
import Dscp.Crypto (hash)
import Dscp.Crypto (Hash, Raw)
import Dscp.Util (Id, fromHex)
import Dscp.Util.Aeson (AsHex)

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

data CourseInfo = CourseInfo
    { ciId         :: !Course
    , ciDesc       :: !Text
    , ciSubjects   :: ![Subject]
    , ciIsEnrolled :: !Bool
    } deriving (Show, Eq, Generic)

data AssignmentInfo = AssignmentInfo
    { aiHash           :: !(Hash Assignment)
    , aiCourseId       :: !Course
    , aiContentsHash   :: !(Hash Raw)
    , aiIsFinal        :: !Bool
    , aiDesc           :: !Text
    , aiLastSubmission :: !(Maybe SubmissionInfo)
    } deriving (Show, Eq, Generic)

data SubmissionInfo = SubmissionInfo
    { siHash           :: !(Hash Submission)
    , siContentsHash   :: !(Hash Raw)
    , siAssignmentHash :: !(Hash Assignment)
    , siGrade          :: !(Maybe GradeInfo)
    } deriving (Show, Eq, Generic)

data GradeInfo = GradeInfo
    { giSubmissionHash :: !(Hash Submission)
    , giGrade          :: !Grade
    , giTimestamp      :: !UTCTime
    , giHasProof       :: !Bool
    } deriving (Show, Eq, Generic)

data BlkProofInfo = BlkProofInfo
    { bpiMtreeSerialized :: !(AsHex ByteString)
    , bpiTxs             :: ![PrivateTx]
    } deriving (Show, Eq, Generic)

data NewSubmission = NewSubmission
    { nsAssignmentHash :: !(Hash Assignment)
    , nsContentsHash   :: !(Hash Raw)
    , nsWitness        :: !SubmissionWitness
    } deriving (Show, Eq, Generic)

nsOwner :: NewSubmission -> Id Student
nsOwner = mkAddr . _swKey . nsWitness

data NewCourse = NewCourse
    { ncId       :: !Course
    , ncDesc     :: !Text
    , ncSubjects :: ![Subject]
    } deriving (Show, Eq, Generic)

data NewGrade = NewGrade
    { ngSubmissionHash :: !(Hash Submission)
    , ngGrade          :: !Grade
    }

data NewAssignment = NewAssignment
    { naCourseId     :: !Course
    , naContentsHash :: !(Hash Raw)
    , naIsFinal      :: !Bool
    , naDesc         :: !Text
    } deriving (Show, Eq, Generic)

---------------------------------------------------------------------------
-- Simple functions
---------------------------------------------------------------------------

assignmentTypeRaw :: Iso' AssignmentType IsFinal
assignmentTypeRaw = iso forth back
  where
    back = \case
        IsFinal False -> Regular
        IsFinal True  -> CourseFinal
    forth = \case
        Regular     -> IsFinal False
        CourseFinal -> IsFinal True

liftAssignment :: Assignment -> Maybe SubmissionInfo -> AssignmentInfo
liftAssignment a lastSubmission =
    AssignmentInfo
    { aiHash = hash a
    , aiCourseId = _aCourseId a
    , aiContentsHash = _aContentsHash a
    , aiIsFinal = _aType a ^. assignmentTypeRaw . _IsFinal
    , aiDesc = _aDesc a
    , aiLastSubmission = lastSubmission
    }

liftSubmission :: Submission -> Maybe GradeInfo -> SubmissionInfo
liftSubmission s siGrade =
    SubmissionInfo
    { siHash = hash s
    , siContentsHash = _sContentsHash s
    , siAssignmentHash = hash (_sAssignment s)
    , ..
    }

signedSubmissionToRequest :: SignedSubmission -> NewSubmission
signedSubmissionToRequest sigSub =
    let submission = _ssSubmission sigSub
    in NewSubmission
        { nsAssignmentHash = hash (_sAssignment submission)
        , nsContentsHash = _sContentsHash submission
        , nsWitness = _ssWitness sigSub
        }

aiDocumentType :: AssignmentInfo -> DocumentType
aiDocumentType = documentType . aiContentsHash

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''PrivateTx

deriveJSON defaultOptions ''IsFinal
deriveJSON defaultOptions ''IsEnrolled
deriveJSON defaultOptions ''HasProof
deriveJSON defaultOptions ''CourseInfo
deriveJSON defaultOptions ''AssignmentInfo
deriveJSON defaultOptions ''SubmissionInfo
deriveJSON defaultOptions ''GradeInfo
deriveJSON defaultOptions ''BlkProofInfo
deriveJSON defaultOptions ''NewSubmission

---------------------------------------------------------------------------
-- FromHttpApiData instances
---------------------------------------------------------------------------

deriving instance FromHttpApiData Course
deriving instance FromHttpApiData Subject
deriving instance FromHttpApiData Grade
deriving instance FromHttpApiData IsEnrolled
deriving instance FromHttpApiData IsFinal

instance FromHttpApiData DocumentType where
    parseQueryParam "offline" = Right Offline
    parseQueryParam "online"  = Right Online
    parseQueryParam other     = Left $ "invalid document type: " <> other

instance FromHttpApiData Address where
    parseQueryParam = addrFromText

instance FromHttpApiData (Hash a) where
    parseQueryParam = first toText . fromHex


-- | Datatypes for Student HTTP API

module Dscp.Educator.Web.Student.Types
       ( Course (..)
       , Assignment (..)
       , Submission (..)
       , Grade (..)
       , BlkProof (..)
       , ErrResponse (..)
       ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (UTCTime)

import Dscp.Core.Aeson ()
import qualified Dscp.Core.Types as Core
import Dscp.Crypto (Hash, Raw)
import Dscp.Educator.Txs (PrivateTx (..))
import Dscp.Educator.Web.Student.Error (APIError)
import Dscp.Util.Aeson (AsByteString, Base64Encoded)

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
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''PrivateTx

deriveJSON defaultOptions ''Course
deriveJSON defaultOptions ''Assignment
deriveJSON defaultOptions ''Submission
deriveJSON defaultOptions ''Grade
deriveJSON defaultOptions ''BlkProof
deriveJSON defaultOptions ''ErrResponse

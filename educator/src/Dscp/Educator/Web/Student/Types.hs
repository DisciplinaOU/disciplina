{-# LANGUAGE StrictData #-}

-- | Types specific to student API.

module Dscp.Educator.Web.Student.Types
    (
      -- * Flags
      IsEnrolled (..)

      -- * Requests
    , NewSubmission (..)
    , nsOwner

      -- * Responses
    , CourseStudentInfo (..)
    , AssignmentStudentInfo (..)
    , SubmissionStudentInfo (..)
    , saDocumentType

      -- * Conversions
    , studentLiftAssignment
    , studentLiftSubmission
    , signedSubmissionToRequest
    ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Servant (FromHttpApiData)

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Types
import Dscp.Util

-- | Whether student is enrolled into a course.
newtype IsEnrolled = IsEnrolled { unIsEnrolled :: Bool }
    deriving (Eq, Show)

data NewSubmission = NewSubmission
    { nsAssignmentHash :: (Hash Assignment)
    , nsContentsHash   :: (Hash Raw)
    , nsWitness        :: SubmissionWitness
    } deriving (Show, Eq, Generic)

nsOwner :: NewSubmission -> Id Student
nsOwner = mkAddr . _swKey . nsWitness

data CourseStudentInfo = CourseStudentInfo
    { ciId         :: Course
    , ciDesc       :: Text
    , ciSubjects   :: [Subject]
    , ciIsEnrolled :: Bool
    } deriving (Show, Eq, Generic)

data AssignmentStudentInfo = AssignmentStudentInfo
    { aiHash           :: (Hash Assignment)
    , aiCourseId       :: Course
    , aiContentsHash   :: (Hash Raw)
    , aiIsFinal        :: Bool
    , aiDesc           :: Text
    , aiLastSubmission :: (Maybe SubmissionStudentInfo)
    } deriving (Show, Eq, Generic)

data SubmissionStudentInfo = SubmissionStudentInfo
    { siHash           :: (Hash Submission)
    , siContentsHash   :: (Hash Raw)
    , siAssignmentHash :: (Hash Assignment)
    , siGrade          :: (Maybe GradeInfo)
    } deriving (Show, Eq, Generic)

saDocumentType :: AssignmentStudentInfo -> DocumentType
saDocumentType = documentType . aiContentsHash

---------------------------------------------------------------------------
-- Simple conversions
---------------------------------------------------------------------------

studentLiftAssignment
    :: Assignment -> Maybe SubmissionStudentInfo -> AssignmentStudentInfo
studentLiftAssignment a lastSubmission =
    AssignmentStudentInfo
    { aiHash = hash a
    , aiCourseId = _aCourseId a
    , aiContentsHash = _aContentsHash a
    , aiIsFinal = _aType a ^. assignmentTypeRaw . _IsFinal
    , aiDesc = _aDesc a
    , aiLastSubmission = lastSubmission
    }

studentLiftSubmission :: Submission -> Maybe GradeInfo -> SubmissionStudentInfo
studentLiftSubmission s siGrade =
    SubmissionStudentInfo
    { siHash = hash s
    , siContentsHash = _sContentsHash s
    , siAssignmentHash = _sAssignmentId s
    , ..
    }

signedSubmissionToRequest :: SignedSubmission -> NewSubmission
signedSubmissionToRequest sigSub =
    let submission = _ssSubmission sigSub
    in NewSubmission
        { nsAssignmentHash = _sAssignmentId submission
        , nsContentsHash = _sContentsHash submission
        , nsWitness = _ssWitness sigSub
        }

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''NewSubmission
deriveJSON defaultOptions ''CourseStudentInfo
deriveJSON defaultOptions ''AssignmentStudentInfo
deriveJSON defaultOptions ''SubmissionStudentInfo

---------------------------------------------------------------------------
-- HttpApiData instances
---------------------------------------------------------------------------

deriving instance FromHttpApiData IsEnrolled

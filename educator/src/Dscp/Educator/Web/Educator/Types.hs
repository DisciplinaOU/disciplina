{-# LANGUAGE StrictData #-}

-- | Types specific to educator API.

module Dscp.Educator.Web.Educator.Types
    (
      -- * Requests
      NewCourse (..)
    , NewGrade (..)
    , NewAssignment (..)
    , EnrollStudentToCourse (..)

      -- * Responses
    , CourseEducatorInfo (..)
    , AssignmentEducatorInfo (..)
    , SubmissionEducatorInfo (..)
    , eaDocumentType

      -- * Conversions
    , educatorLiftAssignment
    , educatorLiftSubmission
    , requestToAssignment
    ) where

import Control.Lens (from)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Types

data NewCourse = NewCourse
    { ncId       :: Course
    , ncDesc     :: Maybe Text
    , ncSubjects :: Maybe [Subject]
    } deriving (Show, Eq, Generic)

data NewGrade = NewGrade
    { ngSubmissionHash :: (Hash Submission)
    , ngGrade          :: Grade
    }

data NewAssignment = NewAssignment
    { naCourseId     :: Course
    , naContentsHash :: (Hash Raw)
    , naIsFinal      :: IsFinal
    , naDesc         :: Text
    } deriving (Show, Eq, Generic)

data EnrollStudentToCourse = EnrollStudentToCourse
    { escCourseId :: Course
    } deriving (Show, Eq, Generic)

data CourseEducatorInfo = CourseEducatorInfo
    { ciId       :: Course
    , ciDesc     :: Text
    , ciSubjects :: [Subject]
    } deriving (Show, Eq, Ord, Generic)

data AssignmentEducatorInfo = AssignmentEducatorInfo
    { aiHash         :: (Hash Assignment)
    , aiCourseId     :: Course
    , aiContentsHash :: (Hash Raw)
    , aiIsFinal      :: IsFinal
    , aiDesc         :: Text
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

---------------------------------------------------------------------------
-- ResponseCase instances
---------------------------------------------------------------------------

type instance ResponseCase 'EducatorTag Course     = CourseEducatorInfo
type instance ResponseCase 'EducatorTag Assignment = AssignmentEducatorInfo
type instance ResponseCase 'EducatorTag Submission = SubmissionEducatorInfo

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

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''NewCourse
deriveJSON defaultOptions ''NewGrade
deriveJSON defaultOptions ''NewAssignment
deriveJSON defaultOptions ''EnrollStudentToCourse
deriveJSON defaultOptions ''CourseEducatorInfo
deriveJSON defaultOptions ''AssignmentEducatorInfo
deriveJSON defaultOptions ''SubmissionEducatorInfo

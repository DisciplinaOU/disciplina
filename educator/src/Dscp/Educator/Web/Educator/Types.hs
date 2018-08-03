-- | Types specific to educator API.

module Dscp.Educator.Web.Educator.Types
    (
      -- * Requests
      NewCourse (..)
    , NewGrade (..)
    , NewAssignment (..)

      -- * Responses
    , CourseEducatorInfo (..)
    , AssignmentEducatorInfo (..)
    , SubmissionEducatorInfo (..)
    , eaDocumentType

      -- * Conversions
    , educatorLiftAssignment
    ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Types

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

data CourseEducatorInfo = CourseEducatorInfo
    { ciId       :: !Course
    , ciDesc     :: !Text
    , ciSubjects :: ![Subject]
    } deriving (Show, Eq, Generic)

data AssignmentEducatorInfo = AssignmentEducatorInfo
    { aiHash         :: !(Hash Assignment)
    , aiCourseId     :: !Course
    , aiContentsHash :: !(Hash Raw)
    , aiIsFinal      :: !Bool
    , aiDesc         :: !Text
    } deriving (Show, Eq, Generic)

data SubmissionEducatorInfo = SubmissionEducatorInfo
    { siHash           :: !(Hash Submission)
    , siContentsHash   :: !(Hash Raw)
    , siAssignmentHash :: !(Hash Assignment)
    , siGrade          :: !(Maybe GradeInfo)
    , siWitness        :: !SubmissionWitness
    } deriving (Show, Eq, Generic)

eaDocumentType :: AssignmentEducatorInfo -> DocumentType
eaDocumentType = documentType . aiContentsHash

---------------------------------------------------------------------------
-- Simple conversions
---------------------------------------------------------------------------

educatorLiftAssignment :: Assignment -> AssignmentEducatorInfo
educatorLiftAssignment a =
    AssignmentEducatorInfo
    { aiHash = hash a
    , aiCourseId = _aCourseId a
    , aiContentsHash = _aContentsHash a
    , aiIsFinal = _aType a ^. assignmentTypeRaw . _IsFinal
    , aiDesc = _aDesc a
    }

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''NewCourse
deriveJSON defaultOptions ''NewGrade
deriveJSON defaultOptions ''NewAssignment
deriveJSON defaultOptions ''CourseEducatorInfo
deriveJSON defaultOptions ''AssignmentEducatorInfo
deriveJSON defaultOptions ''SubmissionEducatorInfo

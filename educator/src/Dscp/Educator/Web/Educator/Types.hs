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
import Fmt (build, listF, (+|), (|+))

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Types
import Dscp.Util.Servant (ForResponseLog (..), buildLongResponseList, buildShortResponseList)

data NewStudent = NewStudent
    { nsAddr :: Student
    } deriving (Show, Eq, Generic)

data NewCourse = NewCourse
    { ncId       :: Maybe Course
    , ncDesc     :: Text
    , ncSubjects :: [Subject]
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

data NewStudentCourse = NewStudentCourse
    { nscCourseId :: Course
    } deriving (Show, Eq, Generic)

data NewStudentAssignment = NewStudentAssignment
    { nsaAssignmentHash :: Hash Assignment
    }

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
-- Buildable instances
---------------------------------------------------------------------------

instance Buildable (NewStudent) where
    build (NewStudent{..}) =
      "{ address = " +| nsAddr |+
      " }"

instance Buildable (NewCourse) where
    build (NewCourse{..}) =
      "{ course id = " +| ncId |+
      ", description = " +| ncDesc |+
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

instance Buildable (ForResponseLog [CourseEducatorInfo]) where
    build = buildLongResponseList

instance Buildable (ForResponseLog [AssignmentEducatorInfo]) where
    build = buildShortResponseList

instance Buildable (ForResponseLog [SubmissionEducatorInfo]) where
    build = buildShortResponseList

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''NewStudent
deriveJSON defaultOptions ''NewCourse
deriveJSON defaultOptions ''NewGrade
deriveJSON defaultOptions ''NewAssignment
deriveJSON defaultOptions ''NewStudentCourse
deriveJSON defaultOptions ''NewStudentAssignment
deriveJSON defaultOptions ''CourseEducatorInfo
deriveJSON defaultOptions ''AssignmentEducatorInfo
deriveJSON defaultOptions ''SubmissionEducatorInfo

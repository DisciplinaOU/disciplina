{-# LANGUAGE StrictData #-}

-- | Types specific to student API.

module Dscp.Educator.Web.Student.Types
    (
      -- * Requests
      NewSubmission (..)
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
    , studentSubmissionInfoFromRow
    ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Swagger (ToSchema (..))
import Fmt (blockListF, build, (+|), (|+))
import Servant.Util (type (?:), FilterKind (..), FilteringParamTypesOf, ForResponseLog (..),
                     SortingParamTypesOf, buildListForResponse)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQL.Util
import Dscp.Educator.DB
import Dscp.Educator.Web.Types
import Dscp.Util
import Dscp.Web.Swagger

data NewSubmission = NewSubmission
    { nsAssignmentHash :: (Hash Assignment)
    , nsContentsHash   :: (Hash Raw)
    , nsWitness        :: SubmissionWitness
    } deriving (Show, Eq, Generic)

nsOwner :: NewSubmission -> Id Student
nsOwner = mkAddr . _swKey . nsWitness

data CourseStudentInfo = CourseStudentInfo
    { ciId         :: Course
    , ciDesc       :: ItemDesc
    , ciSubjects   :: [Subject]
    , ciIsEnrolled :: Bool
    , ciIsFinished :: Bool
    } deriving (Show, Eq, Generic)

data AssignmentStudentInfo = AssignmentStudentInfo
    { aiHash           :: (Hash Assignment)
    , aiCourseId       :: Course
    , aiContentsHash   :: (Hash Raw)
    , aiIsFinal        :: IsFinal
    , aiDesc           :: ItemDesc
    , aiLastSubmission :: (Maybe SubmissionStudentInfo)
    } deriving (Show, Eq, Generic)

data SubmissionStudentInfo = SubmissionStudentInfo
    { siHash           :: (Hash Submission)
    , siContentsHash   :: (Hash Raw)
    , siAssignmentHash :: (Hash Assignment)
    , siGrade          :: (Maybe GradeInfo)
    } deriving (Show, Eq, Generic)

saDocumentType :: AssignmentStudentInfo -> DocumentType Assignment
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
    , aiIsFinal = _aType a ^. assignmentTypeRaw
    , aiDesc = _aDesc a
    , aiLastSubmission = lastSubmission
    }

studentLiftSubmission :: Submission -> Maybe GradeInfo -> SubmissionStudentInfo
studentLiftSubmission s siGrade =
    SubmissionStudentInfo
    { siHash = hash s
    , siContentsHash = _sContentsHash s
    , siAssignmentHash = _sAssignmentHash s
    , ..
    }

signedSubmissionToRequest :: SignedSubmission -> NewSubmission
signedSubmissionToRequest sigSub =
    let submission = _ssSubmission sigSub
    in NewSubmission
        { nsAssignmentHash = _sAssignmentHash submission
        , nsContentsHash = _sContentsHash submission
        , nsWitness = _ssWitness sigSub
        }

studentSubmissionInfoFromRow :: (SubmissionRow, Maybe TransactionRow) -> SubmissionStudentInfo
studentSubmissionInfoFromRow (SubmissionRow{..}, mtx) =
    SubmissionStudentInfo
    { siHash = srHash
    , siContentsHash = srContentsHash
    , siAssignmentHash = unpackPk srAssignment
    , siGrade = fmap gradeInfoFromRow mtx
    }

---------------------------------------------------------------------------
-- Buildable instances
---------------------------------------------------------------------------

instance Buildable (NewSubmission) where
    build (NewSubmission{..}) =
      "{ assignment hash = " +| nsAssignmentHash |+
      ", content hash = " +| nsContentsHash |+
      " }"

instance Buildable (CourseStudentInfo) where
    build (CourseStudentInfo{..}) =
      "{ course id = " +| ciId |+
      ", description = " +| ciDesc |+
      ", subjects = " +| blockListF ciSubjects |+
      ", is enrolled =" +| ciIsEnrolled |+
      ", is finished =" +|ciIsFinished |+
      " }"

instance Buildable (AssignmentStudentInfo) where
    build (AssignmentStudentInfo{..}) =
      "{ course id = " +| aiCourseId |+
      ", assignment hash = " +| aiHash |+
      ", description = " +| aiDesc |+
      " }"

instance Buildable (SubmissionStudentInfo) where
    build (SubmissionStudentInfo{..}) =
      "{ submission hash = " +| siHash |+
      ", content hash = " +| siContentsHash |+
      ", assignment hash = " +| siAssignmentHash |+
      " }"

instance Buildable (ForResponseLog CourseStudentInfo) where
    build (ForResponseLog CourseStudentInfo{..}) =
      "{ course id = " +| ciId |+
      " }"

instance Buildable (ForResponseLog AssignmentStudentInfo) where
    build (ForResponseLog AssignmentStudentInfo{..}) =
      "{ hash = " +| aiHash |+
      " }"

instance Buildable (ForResponseLog SubmissionStudentInfo) where
    build (ForResponseLog SubmissionStudentInfo{..}) =
      "{ hash = " +| siHash |+
      " }"

instance Buildable (ForResponseLog [CourseStudentInfo]) where
    build = buildListForResponse (take 4)

instance Buildable (ForResponseLog [AssignmentStudentInfo]) where
    build = buildListForResponse (take 4)

instance Buildable (ForResponseLog [SubmissionStudentInfo]) where
    build = buildListForResponse (take 4)

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''NewSubmission
deriveJSON defaultOptions ''CourseStudentInfo
deriveJSON defaultOptions ''AssignmentStudentInfo
deriveJSON defaultOptions ''SubmissionStudentInfo

---------------------------------------------------------------------------
-- Swagger instances
---------------------------------------------------------------------------

instance ToSchema NewSubmission where
    declareNamedSchema p =
        inDeclaredSchema (gDeclareNamedSchema p) $
            setExample $ signedSubmissionToRequest signedSubmissionEx

instance ToSchema CourseStudentInfo where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema AssignmentStudentInfo where
    declareNamedSchema = gDeclareNamedSchema

instance ToSchema SubmissionStudentInfo where
    declareNamedSchema = gDeclareNamedSchema

---------------------------------------------------------------------------
-- Sorting parameters
---------------------------------------------------------------------------

type instance SortingParamTypesOf CourseStudentInfo =
    ["id" ?: Course, "desc" ?: ItemDesc]

-- TODO [DSCP-424]: add timestamps
type instance SortingParamTypesOf AssignmentStudentInfo =
    ["course" ?: Course, "desc" ?: ItemDesc]

-- TODO [DSCP-424]: add timestamps
type instance SortingParamTypesOf SubmissionStudentInfo =
    '["grade" ?: Maybe Grade]

---------------------------------------------------------------------------
-- Filtering parameters
---------------------------------------------------------------------------

type instance FilteringParamTypesOf CourseStudentInfo =
    [ "isEnrolled" ?: 'ManualFilter IsEnrolled
    , "desc" ?: 'AutoFilter ItemDesc
    ]

-- TODO [DSCP-424]: add timestamps
type instance FilteringParamTypesOf AssignmentStudentInfo =
    [ "course" ?: 'AutoFilter Course
    , "docType" ?: 'ManualFilter (DocumentType Assignment)
    , "isFinal" ?: 'ManualFilter IsFinal
    , "desc" ?: 'AutoFilter ItemDesc
    ]

-- TODO [DSCP-424]: add timestamps
type instance FilteringParamTypesOf SubmissionStudentInfo =
    [ "course" ?: 'AutoFilter Course
    , "assignmentHash" ?: 'AutoFilter (Hash Assignment)
    , "docType" ?: 'ManualFilter (DocumentType Submission)
    ]

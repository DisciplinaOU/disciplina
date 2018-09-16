{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Common queries for student and educator APIs.

module Dscp.Educator.Web.Queries
    ( module Dscp.Educator.Web.Queries
    ) where

import Control.Lens (from, mapping)
import Database.SQLite.Simple (Only (..), (:.) (..))
import Loot.Log (MonadLogging)
import Data.Default (Default (..))
import Text.InterpolatedString.Perl6 (q, qc)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQLite
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Student.Types
import Dscp.Educator.Web.Student.Queries -- remove
import Dscp.Educator.Web.Types
import Dscp.Util
import Dscp.Util.Type (type (==))

type MonadEducatorQuery m =
    ( MonadIO m
    , MonadCatch m
    , MonadLogging m
    )

----------------------------------------------------------------------------
-- Assignments
----------------------------------------------------------------------------

data GetAssignmentsFilters = GetAssignmentsFilters
    { afAssignmentHash :: Maybe $ Hash Assignment
    , afCourse         :: Maybe Course
    , afDocType        :: Maybe DocumentType
    , afIsFinal        :: Maybe IsFinal
    } deriving (Show, Generic)

instance Default GetAssignmentsFilters where
    def = GetAssignmentsFilters def def def def

commonGetAssignments
    :: (MonadEducatorQuery m, DistinctTag apiTag)
    => ApiCase apiTag
    -> Student
    -> GetAssignmentsFilters
    -> DBT 'WithinTx w m [ResponseCase apiTag Assignment]
commonGetAssignments apiCase student filters = do
    assignments <- query queryText (mconcat $ oneParam student : paramsF)
    forM assignments $
        \( assignH        :: Hash Assignment
         , aiCourseId     :: Course
         , aiContentsHash :: Hash Raw
         , assignType     :: AssignmentType
         , aiDesc         :: Text
         ) -> do
        let aiIsFinal = assignType ^. assignmentTypeRaw
        case apiCase of
            EducatorCase ->
                return AssignmentEducatorInfo{ aiHash = assignH, .. }
            StudentCase -> do
                aiLastSubmission <- studentGetLastAssignmentSubmission student assignH
                return AssignmentStudentInfo{ aiHash = assignH, .. }
  where
    (clausesF, paramsF) = unzip
        [ mkFilter "Assignments.hash = ?" (afAssignmentHash filters)
        , mkFilter "course_id = ?" (afCourse filters)
        , mkDocTypeFilter "Assignments.hash" (afDocType filters)
        , let assignTypeF = afIsFinal filters ^. mapping (from assignmentTypeRaw)
          in mkFilter "type = ?" assignTypeF
        ]

    queryText = [q|
        select    hash, course_id, contents_hash, type, desc
        from      Assignments
        left join StudentAssignments
               on StudentAssignments.assignment_hash = Assignments.hash
        where     student_addr = ?
    |]
      `filterClauses` clausesF

----------------------------------------------------------------------------
-- Submissions
----------------------------------------------------------------------------

data GetSubmissionsFilters = GetSubmissionsFilters
    { sfStudent        :: Maybe Student
    , sfCourse         :: Maybe Course
    , sfSubmissionHash :: Maybe $ Hash Submission
    , sfAssignmentHash :: Maybe $ Hash Assignment
    , sfDocType        :: Maybe DocumentType
    } deriving (Show, Generic)

instance Default GetSubmissionsFilters where
    def = GetSubmissionsFilters def def def def def

commonGetSubmissions
    :: forall apiTag m t w.
       (MonadEducatorQuery m, DistinctTag apiTag)
    => ApiCase apiTag
    -> GetSubmissionsFilters
    -> DBT t w m [ResponseCase apiTag Submission]
commonGetSubmissions apiCase filters = do
    submissions <- query queryText (mconcat paramsF)
    return $ submissions <&>
      \(  (submissionH      :: Hash Submission
       ,   siContentsHash   :: Hash Raw
       ,   siAssignmentHash :: Hash Assignment
          )
       :. (witness          :: FetchIf (apiTag == 'EducatorTag)
                                       (Only SubmissionWitness))
       :. (siGrade          :: Maybe GradeInfo)
       ) ->
        case apiCase of
            StudentCase  -> SubmissionStudentInfo
                            { siHash = submissionH, .. }
            EducatorCase -> SubmissionEducatorInfo
                            { siHash = submissionH
                            , siWitness = fromOnly $ positiveFetch witness, .. }
  where
    (clausesF, paramsF) = unzip
        [ mkFilter "S.student_addr = ?" (sfStudent filters)
        , mkFilter "course_id = ?" (sfCourse filters)
        , mkFilter "A.hash = ?" (sfAssignmentHash filters)
        , mkFilter "S.hash = ?" (sfSubmissionHash filters)
        , mkDocTypeFilter "A.hash" (sfDocType filters)
        ]

    extraFields :: Text
    extraFields = case apiCase of
        StudentCase  -> ""
        EducatorCase -> "S.signature,"

    queryText = [qc|
        select    S.hash, S.contents_hash, assignment_hash, {extraFields}
                  T.submission_hash, T.grade, T.time, T.idx  -- grade
        from      Submissions as S
        left join Assignments as A
               on A.hash = S.assignment_hash
        left join Transactions as T
               on T.submission_hash = S.hash
        where     1 = 1
    |]
      `filterClauses` clausesF

----------------------------------------------------------------------------
-- Predicates
----------------------------------------------------------------------------

commonExistsSubmission
    :: (MonadEducatorWebQuery m)
    => Hash Submission
    -> Maybe Student
    -> DBT t w m Bool
commonExistsSubmission submissionH studentF = do
    checkExists queryText (oneParam submissionH <> paramF)
  where
    (clauseF, paramF) = mkFilter "student_addr = ?" studentF
    queryText = [q|
       select   count(*)
       from     Submissions
       where    hash = ?
    |]
      `filterClauses` one clauseF

----------------------------------------------------------------------------
-- Deletions
----------------------------------------------------------------------------

commonDeleteSubmission
    :: (MonadEducatorWebQuery m)
    => Hash Submission
    -> Maybe Student
    -> DBT 'WithinTx 'Writing m ()
commonDeleteSubmission submissionH studentF = do
    commonExistsSubmission submissionH studentF
        `assert` AbsentError (SubmissionDomain submissionH)
    execute queryText (oneParam submissionH <> paramF)
        `onReferenceInvalidThrow`
        (SemanticError $ DeletingGradedSubmission submissionH)
  where
    (clauseF, paramF) = mkFilter "student_addr = ?" studentF
    queryText = [q|
       delete
       from     Submissions
       where    hash = ?
    |]
      `filterClauses` one clauseF

----------------------------------------------------------------------------
-- Filters
----------------------------------------------------------------------------

-- | Create filter for 'DocumentType'.
mkDocTypeFilter :: String -> Maybe DocumentType -> (FilterClause, SomeParams)
mkDocTypeFilter fieldName = \case
    Nothing ->
        ("", mempty)
    Just Offline ->
        (FilterClause $ "and " <> fieldName <> " = ?", oneParam offlineHash)
    Just Online  ->
        (FilterClause $ "and " <> fieldName <> " <> ?", oneParam offlineHash)

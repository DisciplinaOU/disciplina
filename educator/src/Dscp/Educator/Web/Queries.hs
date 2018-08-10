{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Common queries for student and educator APIs.

module Dscp.Educator.Web.Queries
    ( module Dscp.Educator.Web.Queries
    ) where

import Control.Lens (from, mapping)
import Data.Time.Clock (UTCTime)
import Dscp.Util.Aeson (AsByteString (..))
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
import Dscp.Educator.Web.Util
import Dscp.Util
import Dscp.Util.Type (type (==))

type MonadEducatorQuery m =
    ( MonadSQLiteDB m
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
    :: (MonadEducatorQuery m, DistinctTag apiTag, WithinSQLTransaction)
    => ApiCase apiTag
    -> Student
    -> GetAssignmentsFilters
    -> m [ResponseCase apiTag Assignment]
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
    assignFilter = mkFilterOn "Assignments.hash" (afAssignmentHash filters)
    courseFilter = mkFilterOn "course_id" (afCourse filters)
    assignTypeF = afIsFinal filters ^. mapping (from assignmentTypeRaw)
    assignTypeFilter = mkFilterOn "type" assignTypeF
    docTypeFilter = mkDocTypeFilter "Assignments.hash" (afDocType filters)
    (clausesF, paramsF) =
        unzip [assignFilter, courseFilter, assignTypeFilter, docTypeFilter]

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
    :: forall apiTag m.
       (MonadEducatorQuery m, DistinctTag apiTag)
    => ApiCase apiTag
    -> GetSubmissionsFilters
    -> m [ResponseCase apiTag Submission]
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
    studentFilter = mkFilterOn "S.student_addr" (sfStudent filters)
    courseFilter  = mkFilterOn "course_id" (sfCourse filters)
    assignFilter  = mkFilterOn "A.hash" (sfAssignmentHash filters)
    subFilter     = mkFilterOn "S.hash" (sfSubmissionHash filters)
    docTypeFilter = mkDocTypeFilter "A.hash" (sfDocType filters)
    (clausesF, paramsF) =
        unzip [studentFilter, courseFilter, assignFilter, subFilter, docTypeFilter]

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
-- Proofs
----------------------------------------------------------------------------

-- | Given db-internal block index, fetch transactions it contains.
commonGetBlockTxs
    :: MonadStudentAPIQuery m
    => Student
    -> TxBlockIdx
    -> m [PrivateTx]
commonGetBlockTxs student blockIdx = do
    query queryText (blockIdx, student)
  where
    queryText = [q|
        select     Submissions.student_addr,
                   Submissions.contents_hash,
                   Assignments.hash,

                   Submissions.signature,
                   grade,
                   time

        from       Transactions

        left join  Submissions
               on  submission_hash = Submissions.hash

        left join  Assignments
               on  assignment_hash = Assignments.hash

        where      Transactions.idx = ?
              and  student_addr = ?
    |]

data GetProofsFilters = GetProofsFilters
    { pfCourse :: Maybe Course
    , pfSince  :: Maybe UTCTime
    } deriving (Show, Generic)

instance Default GetProofsFilters where
    def = GetProofsFilters def def

-- | Get proofs of student activity grouped by blocks.
commonGetProofs
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student
    -> GetProofsFilters
    -> m [BlkProofInfo]
commonGetProofs student filters = do
    -- It's questionable whether doing just one request would be optimal here
    -- since fetching same merkel-tree for each transaction in block can take long.
    blocks <- query queryText (pfCourse filters, pfCourse filters, pfSince filters, pfSince filters)
    forM blocks $ \(idx, tree) -> do
        txs <- commonGetBlockTxs student idx
        return BlkProofInfo
            { bpiMtreeSerialized = AsByteString tree
            , bpiTxs = txs
            }
  where
    queryText = [q|
        select     Blocks.idx, mtree
        from       Blocks
        left join  Transactions
                on Transactions.idx = Blocks.idx
        left join  Submissions
                on Transactions.submission_hash = Submissions.hash
        left join  Assignments
                on Submissions.assignment_hash = Assignments.hash
        where      Transactions.hash is not null  -- nulls arise from join
               and (? is null or Assignments.course_id = ?)
               and (? is null or Blocks.time >= ?)
    |]

----------------------------------------------------------------------------
-- Predicates
----------------------------------------------------------------------------

commonExistsSubmission
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Hash Submission
    -> Maybe Student
    -> m Bool
commonExistsSubmission submissionH studentF = do
    checkExists queryText (oneParam submissionH <> paramF)
  where
    (clauseF, paramF) = mkFilterOn "student_addr" studentF
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
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Hash Submission
    -> Maybe Student
    -> m ()
commonDeleteSubmission submissionH studentF = do
    commonExistsSubmission submissionH studentF
        `assert` AbsentError (SubmissionDomain submissionH)
    execute queryText (oneParam submissionH <> paramF)
        `onReferenceInvalidThrow` (SemanticError $ DeletingGradedSubmission submissionH)
  where
    (clauseF, paramF) = mkFilterOn "student_addr" studentF
    queryText = [q|
       delete
       from     Submissions
       where    hash = ?
    |]
      `filterClauses` one clauseF

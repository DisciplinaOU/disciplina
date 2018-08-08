{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TypeOperators             #-}

module Dscp.Educator.Web.Student.Queries
    ( module Dscp.Educator.Web.Student.Queries
    ) where

import Control.Exception.Safe (catchJust)
import Control.Lens (from, mapping)
import Data.Coerce (coerce)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple ((:.) (..), Only (..), Query, ToRow (..))
import Database.SQLite.Simple.ToField (ToField)
import Loot.Log (MonadLogging)
import Text.InterpolatedString.Perl6 (q)

import Dscp.Core
import Dscp.Crypto (Hash, hash)
import Dscp.DB.SQLite (DomainError (..), MonadSQLiteDB (..), TxBlockIdx (TxInMempool),
                       WithinSQLTransaction)
import qualified Dscp.DB.SQLite.Queries as Base
import Dscp.DB.SQLite.Types (asAlreadyExistsError)
import Dscp.Util (Id, assertJust, listToMaybeWarn)
import Dscp.Util.Aeson (AsByteString (..))

import Dscp.Educator.Web.Student.Error (APIError (..), ObjectAlreadyExistsError (..))
import Dscp.Educator.Web.Student.Types
import Dscp.Educator.Web.Types

---------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------

-- | One of conditions within conjenction sum after @where@.
newtype FilterClause = FilterClause { unFilterClause :: String }
    deriving (IsString)

-- | Contains a list of objects forming a row in SQL table.
data SomeParams = forall a. ToRow a => SomeParams a

instance Monoid SomeParams where
    mempty = SomeParams ()
    SomeParams a `mappend` SomeParams b = SomeParams (a :. b)

instance ToRow SomeParams where
    toRow (SomeParams a) = toRow a

-- | Lift a single field to 'SomeParams'.
oneParam :: ToField a => a -> SomeParams
oneParam = SomeParams . Only

-- | For SQL table field and optional value, makes a clause to select
-- only rows with field mathing value (if present).
-- This function helps to deal with sqlite feature of passing parameters
-- seperately from query itself.
mkFilterOn :: ToField a => String -> Maybe a -> (FilterClause, SomeParams)
mkFilterOn fieldName mparam = case mparam of
      Just param -> (FilterClause ("and " <> fieldName <> "= ?"),
                     oneParam param)
      Nothing    -> ("", mempty)

-- | Attaches filtering @where@ clauses to query.
-- "where" statement with some condition should go last in the query.
filterClauses :: Query -> [FilterClause] -> Query
filterClauses queryText fs =
    mconcat $ queryText : map (fromString . unFilterClause) fs
infixl 1 `filterClauses`

---------------------------------------------------------------------
-- Queries
---------------------------------------------------------------------

type MonadStudentAPIQuery m =
    ( MonadSQLiteDB m
    , MonadLogging m
    , MonadCatch m
    )

studentGetCourse
    :: MonadStudentAPIQuery m
    => Id Student -> Course -> m CourseStudentInfo
studentGetCourse studentId courseId =
    transaction $ do
        mcourse <-
            query queryText (Only courseId)
            >>= listToMaybeWarn "courses"
        Only mdesc <-
            pure mcourse `assertJust` CourseDoesNotExist courseId

        ciIsEnrolled <- Base.isEnrolledTo studentId courseId
        ciSubjects <- Base.getCourseSubjects courseId
        let ciDesc = fromMaybe "" mdesc
        return CourseStudentInfo{ ciId = courseId, .. }
  where
    queryText :: Query
    queryText = [q|
        select    desc
        from      Courses
        where     id = ?
    |]

studentGetCourses
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Id Student -> Maybe IsEnrolled -> m [CourseStudentInfo]
studentGetCourses studentId (coerce -> isEnrolledF) = do
    let (enrolledClause, enrolledParam) = case isEnrolledF of
            Just isEnrolled -> (mkEnrolledClause isEnrolled, oneParam studentId)
            Nothing         -> ("", mempty)

    courses <- query (queryText <> enrolledClause)
                      enrolledParam

    forM courses $ \(courceId, mdesc) -> do
        ciIsEnrolled <- Base.isEnrolledTo studentId courceId
        ciSubjects <- Base.getCourseSubjects courceId
        let ciDesc = fromMaybe "" mdesc
        return CourseStudentInfo{ ciId = courceId, .. }
  where
    queryText :: Query
    queryText = [q|
        select distinct Courses.id, Courses.desc
        from            Courses
    |]
    mkEnrolledClause sign =
        "where " <> bool "not" "" sign <> [q| exists
        (
            select *
            from   StudentCourses
            where  StudentCourses.course_id = Courses.id
               and StudentCourses.student_addr = ?
        )
    |]

studentGetGrade :: MonadStudentAPIQuery m => Hash Submission -> m (Maybe GradeInfo)
studentGetGrade submissionH = do
    mgrade <-
        query queryText (Only submissionH)
        >>= listToMaybeWarn "last submission"
    forM mgrade $ \(giGrade, giTimestamp, giSubmissionHash, blkIdx) -> do
        let giHasProof = blkIdx /= TxInMempool
        return GradeInfo{..}
  where
    queryText :: Query
    queryText = [q|
        select    grade, time, submission_hash, idx
        from      Transactions
        where     submission_hash = ?
    |]

studentGetLastAssignmentSubmission
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student -> Hash Assignment -> m (Maybe SubmissionStudentInfo)
studentGetLastAssignmentSubmission student assignH = do
    msubmission <-
        query queryText (assignH, student)
        >>= listToMaybeWarn "last submission"
    forM msubmission $ \(submissionH, siContentsHash, siAssignmentHash) -> do
        siGrade <- studentGetGrade submissionH
        return SubmissionStudentInfo{ siHash = submissionH, .. }
  where
    queryText :: Query
    queryText = [q|
        select    hash, contents_hash, assignment_hash
        from      Submissions
        where     assignment_hash = ?
              and student_addr = ?
        order by  creation_time desc
        limit     1
    |]

studentGetAssignment
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student -> Hash Assignment -> m AssignmentStudentInfo
studentGetAssignment student assignH = do
    massign <-
        query queryText (assignH, student)
        >>= listToMaybeWarn "assignments"
    (aiCourseId, aiContentsHash, assignType, aiDesc) <-
        pure massign `assertJust` AssignmentDoesNotExist assignH
    let IsFinal aiIsFinal = assignType ^. assignmentTypeRaw
    aiLastSubmission <- studentGetLastAssignmentSubmission student assignH
    return AssignmentStudentInfo{ aiHash = assignH, .. }
  where
    queryText :: Query
    queryText = [q|
        select    course_id, contents_hash, type, desc
        from      Assignments
        left join StudentAssignments
               on StudentAssignments.assignment_hash = Assignments.hash
        where     hash = ?
              and student_addr = ?
    |]

mkDocTypeFilter :: Maybe DocumentType -> (FilterClause, SomeParams)
mkDocTypeFilter = \case
    Nothing ->
        ("", mempty)
    Just Offline ->
        ("and Assignments.hash = ?", oneParam offlineHash)
    Just Online  ->
        ("and Assignments.hash <> ?", oneParam offlineHash)

studentGetAssignments
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student
    -> Maybe Course
    -> Maybe DocumentType
    -> Maybe IsFinal
    -> m [AssignmentStudentInfo]
studentGetAssignments student courseIdF docTypeF isFinalF = do
    let courseFilter = mkFilterOn "course_id" courseIdF
        assignTypeF = isFinalF ^. mapping (from assignmentTypeRaw)
        assignTypeFilter = mkFilterOn "type" assignTypeF
        docTypeFilter = mkDocTypeFilter docTypeF
        (clausesF, paramsF) =
            unzip [courseFilter, assignTypeFilter, docTypeFilter]

    assignments <- query (queryText clausesF)
                         (mconcat $ oneParam student : paramsF)
    forM assignments $
      \(assignH, aiCourseId, aiContentsHash, assignType, aiDesc) -> do
        let IsFinal aiIsFinal = assignType ^. assignmentTypeRaw
        aiLastSubmission <- studentGetLastAssignmentSubmission student assignH
        return AssignmentStudentInfo{ aiHash = assignH, .. }
  where
    queryText clausesF = [q|
        select    hash, course_id, contents_hash, type, desc
        from      Assignments
        left join StudentAssignments
               on StudentAssignments.assignment_hash = Assignments.hash
        where     student_addr = ?
    |]
      `filterClauses` clausesF

studentGetSubmission
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student -> Hash Submission -> m SubmissionStudentInfo
studentGetSubmission student submissionH = do
    msubmission <-
        query queryText (submissionH, student)
        >>= listToMaybeWarn "courses"
    (siContentsHash, siAssignmentHash) <-
        pure msubmission `assertJust` SubmissionDoesNotExist submissionH
    siGrade <- studentGetGrade submissionH
    return SubmissionStudentInfo{ siHash = submissionH, .. }
  where
    queryText :: Query
    queryText = [q|
        select    contents_hash, assignment_hash
        from      Submissions
        where     hash = ?
              and student_addr = ?
    |]

studentGetSubmissions
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student
    -> Maybe Course
    -> Maybe (Hash Assignment)
    -> Maybe DocumentType
    -> m [SubmissionStudentInfo]
studentGetSubmissions student courseIdF assignHF docTypeF = do
    let courseFilter = mkFilterOn "course_id" courseIdF
        assignFilter = mkFilterOn "Assignments.hash" assignHF
        docTypeFilter = mkDocTypeFilter docTypeF
        (clausesF, paramsF) = unzip [courseFilter, assignFilter, docTypeFilter]

    submissions <- query (queryText clausesF)
                         (mconcat $ oneParam student : paramsF)
    forM submissions $
      \(submissionH, siContentsHash, siAssignmentHash) -> do
        siGrade <- studentGetGrade submissionH
        return SubmissionStudentInfo{ siHash = submissionH, .. }
  where
    queryText clausesF = [q|
        select    Submissions.hash, Submissions.contents_hash, assignment_hash
        from      Submissions
        left join Assignments
               on Assignments.hash = Submissions.assignment_hash
        where     Submissions.student_addr = ?
    |]
      `filterClauses` clausesF

studentEnsureSubmissionExists
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student -> Hash Submission -> m ()
studentEnsureSubmissionExists = void ... studentGetSubmission

studentDeleteSubmission
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student
    -> Hash Submission
    -> m ()
studentDeleteSubmission student submissionH = do
    submission <- studentGetSubmission student submissionH
    whenJust (siGrade submission) $ \_ ->
        throwM DeletingGradedSubmission

    execute queryText (submissionH, student)
  where
    queryText = [q|
       delete
       from     Submissions
       where    hash = ?
            and student_addr = ?
    |]

studentMakeSubmission
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => SignedSubmission -> m (Id Submission)
studentMakeSubmission signedSubmission =
    Base.submitAssignment signedSubmission
        & handleAlreadyPresent
  where
    handleAlreadyPresent action =
        catchJust asAlreadyExistsError action $ \_ -> do
            let subH = hash (_ssSubmission signedSubmission)
            throwM $ EntityAlreadyPresent (SubmissionAlreadyExists subH)

studentGetBlockTxs
    :: MonadStudentAPIQuery m
    => Student
    -> Word32
    -> m [PrivateTx]
studentGetBlockTxs student blockIdx = do
    query queryText (blockIdx, student)
  where
    queryText :: Query
    queryText = [q|
        select     Submissions.student_addr,
                   Submissions.contents_hash,
                   Assignments.course_id,

                   Assignments.contents_hash,
                   Assignments.type,
                   Assignments.desc,

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

studentGetProofs
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student
    -> Maybe UTCTime
    -> m [BlkProofInfo]
studentGetProofs student sinceF = do
    blocks <- query queryText (Only sinceF)
    forM blocks $ \(idx, tree) -> do
        txs <- studentGetBlockTxs student idx
        return BlkProofInfo
            { bpiMtreeSerialized = AsByteString tree
            , bpiTxs = txs
            }
  where
    queryText :: Query
    queryText = [q|
        select     idx, mtree
        from       Blocks
        where      time >= ?
    |]

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

import Dscp.Core.Foundation.Educator.Txs (PrivateTx)
import Dscp.Core.Serialise ()
import qualified Dscp.Core.Types as Core
import Dscp.Crypto (Hash, hash)
import Dscp.DB.SQLite (DomainError (..), MonadSQLiteDB (..), TxBlockIdx (TxInMempool),
                       WithinSQLTransaction)
import qualified Dscp.DB.SQLite.Queries as Base
import Dscp.DB.SQLite.Types (asAlreadyExistsError)
import Dscp.Util (Id, assertJust, listToMaybeWarn)
import Dscp.Util.Aeson (AsByteString (..))

import Dscp.Educator.Web.Student.Error (APIError (..), ObjectAlreadyExistsError (..))
import Dscp.Educator.Web.Student.Types (Assignment (..), BlkProof (..), Course (..), Grade (..),
                                        IsEnrolled (..), IsFinal (..), Student, Submission (..),
                                        assignmentTypeRaw)


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

getCourse
    :: MonadStudentAPIQuery m
    => Id Student -> Core.Course -> m Course
getCourse studentId courseId =
    transaction $ do
        mcourse <-
            query queryText (Only courseId)
            >>= listToMaybeWarn "courses"
        Only mdesc <-
            pure mcourse `assertJust` CourseDoesNotExist courseId

        cIsEnrolled <- Base.isEnrolledTo studentId courseId
        cSubjects <- Base.getCourseSubjects courseId
        let cDesc = fromMaybe "" mdesc
        return Course{ cId = courseId, .. }
  where
    queryText :: Query
    queryText = [q|
        select    desc
        from      Courses
        where     id = ?
    |]

getCourses
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Id Student -> Maybe IsEnrolled -> m [Course]
getCourses studentId (coerce -> isEnrolledF) = do
    let (enrolledClause, enrolledParam) = case isEnrolledF of
            Just isEnrolled -> (mkEnrolledClause isEnrolled, oneParam studentId)
            Nothing         -> ("", mempty)

    courses <- query (queryText <> enrolledClause)
                      enrolledParam

    forM courses $ \(courceId, mdesc) -> do
        cIsEnrolled <- Base.isEnrolledTo studentId courceId
        cSubjects <- Base.getCourseSubjects courceId
        let cDesc = fromMaybe "" mdesc
        return Course{ cId = courceId, .. }
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

getGrade :: MonadStudentAPIQuery m => Hash Core.Submission -> m (Maybe Grade)
getGrade submissionH = do
    mgrade <-
        query queryText (Only submissionH)
        >>= listToMaybeWarn "last submission"
    forM mgrade $ \(gGrade, gTimestamp, blkIdx) -> do
        let gHasProof = blkIdx /= TxInMempool
        return Grade{..}
  where
    queryText :: Query
    queryText = [q|
        select    grade, time, idx
        from      Transactions
        where     submission_hash = ?
    |]

getLastAssignmentSubmission
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student -> Hash Core.Assignment -> m (Maybe Submission)
getLastAssignmentSubmission student assignH = do
    msubmission <-
        query queryText (assignH, student)
        >>= listToMaybeWarn "last submission"
    forM msubmission $ \(submissionH, sContentsHash, sAssignmentHash) -> do
        sGrade <- getGrade submissionH
        return Submission{ sHash = submissionH, .. }
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

getAssignment
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student -> Hash Core.Assignment -> m Assignment
getAssignment student assignH = do
    massign <-
        query queryText (assignH, student)
        >>= listToMaybeWarn "assignments"
    (aCourseId, aContentsHash, assignType, aDesc) <-
        pure massign `assertJust` AssignmentDoesNotExist assignH
    let IsFinal aIsFinal = assignType ^. assignmentTypeRaw
    aLastSubmission <- getLastAssignmentSubmission student assignH
    return Assignment{ aHash = assignH, .. }
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

mkDocTypeFilter :: Maybe Core.DocumentType -> (FilterClause, SomeParams)
mkDocTypeFilter = \case
    Nothing ->
        ("", mempty)
    Just Core.Offline ->
        ("and Assignments.hash = ?", oneParam Core.offlineHash)
    Just Core.Online  ->
        ("and Assignments.hash <> ?", oneParam Core.offlineHash)

getAssignments
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student
    -> Maybe Core.Course
    -> Maybe Core.DocumentType
    -> Maybe IsFinal
    -> m [Assignment]
getAssignments student courseIdF docTypeF isFinalF = do
    let courseFilter = mkFilterOn "course_id" courseIdF
        assignTypeF = isFinalF ^. mapping (from assignmentTypeRaw)
        assignTypeFilter = mkFilterOn "type" assignTypeF
        docTypeFilter = mkDocTypeFilter docTypeF
        (clausesF, paramsF) =
            unzip [courseFilter, assignTypeFilter, docTypeFilter]

    assignments <- query (queryText clausesF)
                         (mconcat $ oneParam student : paramsF)
    forM assignments $
      \(assignH, aCourseId, aContentsHash, assignType, aDesc) -> do
        let IsFinal aIsFinal = assignType ^. assignmentTypeRaw
        aLastSubmission <- getLastAssignmentSubmission student assignH
        return Assignment{ aHash = assignH, .. }
  where
    queryText clausesF = [q|
        select    hash, course_id, contents_hash, type, desc
        from      Assignments
        left join StudentAssignments
               on StudentAssignments.assignment_hash = Assignments.hash
        where     student_addr = ?
    |]
      `filterClauses` clausesF

getSubmission
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student -> Hash Core.Submission -> m Submission
getSubmission student submissionH = do
    msubmission <-
        query queryText (submissionH, student)
        >>= listToMaybeWarn "courses"
    (sContentsHash, sAssignmentHash) <-
        pure msubmission `assertJust` SubmissionDoesNotExist submissionH
    sGrade <- getGrade submissionH
    return Submission{ sHash = submissionH, .. }
  where
    queryText :: Query
    queryText = [q|
        select    contents_hash, assignment_hash
        from      Submissions
        where     hash = ?
              and student_addr = ?
    |]

getSubmissions
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student
    -> Maybe Core.Course
    -> Maybe (Hash Core.Assignment)
    -> Maybe Core.DocumentType
    -> m [Submission]
getSubmissions student courseIdF assignHF docTypeF = do
    let courseFilter = mkFilterOn "course_id" courseIdF
        assignFilter = mkFilterOn "Assignments.hash" assignHF
        docTypeFilter = mkDocTypeFilter docTypeF
        (clausesF, paramsF) = unzip [courseFilter, assignFilter, docTypeFilter]

    submissions <- query (queryText clausesF)
                         (mconcat $ oneParam student : paramsF)
    forM submissions $
      \(submissionH, sContentsHash, sAssignmentHash) -> do
        sGrade <- getGrade submissionH
        return Submission{ sHash = submissionH, .. }
  where
    queryText clausesF = [q|
        select    Submissions.hash, Submissions.contents_hash, assignment_hash
        from      Submissions
        left join Assignments
               on Assignments.hash = Submissions.assignment_hash
        where     Submissions.student_addr = ?
    |]
      `filterClauses` clausesF

ensureSubmissionExists
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student -> Hash Core.Submission -> m ()
ensureSubmissionExists = void ... getSubmission

deleteSubmission
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student
    -> Hash Core.Submission
    -> m ()
deleteSubmission student submissionH = do
    submission <- getSubmission student submissionH
    whenJust (sGrade submission) $ \_ ->
        throwM DeletingGradedSubmission

    execute queryText (submissionH, student)
  where
    queryText = [q|
       delete
       from     Submissions
       where    hash = ?
            and student_addr = ?
    |]

makeSubmission
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Core.SignedSubmission -> m (Id Core.Submission)
makeSubmission signedSubmission =
    Base.submitAssignment signedSubmission
        & handleAlreadyPresent
  where
    handleAlreadyPresent action =
        catchJust asAlreadyExistsError action $ \_ -> do
            let subH = hash (Core._ssSubmission signedSubmission)
            throwM $ EntityAlreadyPresent (SubmissionAlreadyExists subH)

getBlockTxs
    :: MonadStudentAPIQuery m
    => Student
    -> Word32
    -> m [PrivateTx]
getBlockTxs student blockIdx = do
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

getProofs
    :: (MonadStudentAPIQuery m, WithinSQLTransaction)
    => Student
    -> Maybe UTCTime
    -> m [BlkProof]
getProofs student sinceF = do
    blocks <- query queryText (Only sinceF)
    forM blocks $ \(idx, tree) -> do
        txs <- getBlockTxs student idx
        return BlkProof
            { bpMtreeSerialized = AsByteString tree
            , bpTxs = txs
            }
  where
    queryText :: Query
    queryText = [q|
        select     idx, mtree
        from       Blocks
        where      time >= ?
    |]

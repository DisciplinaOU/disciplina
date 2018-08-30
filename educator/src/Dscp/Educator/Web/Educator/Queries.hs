{-# LANGUAGE QuasiQuotes #-}

module Dscp.Educator.Web.Educator.Queries
    ( module Dscp.Educator.Web.Educator.Queries
    ) where

import Control.Lens (from, mapping, traversed, _Just)
import Data.List (groupBy)
import Data.Time.Clock (getCurrentTime)
import Database.SQLite.Simple (Only (..), Query)
import Loot.Log (MonadLogging)
import Servant (err501)
import Text.InterpolatedString.Perl6 (q)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQLite
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Types
import Dscp.Educator.Web.Util
import Dscp.Util

type MonadEducatorAPIQuery m =
    ( MonadSQLiteDB m
    , MonadCatch m
    , MonadLogging m
    )

educatorRemoveStudent :: MonadEducatorAPIQuery m => Student -> m ()
educatorRemoveStudent student = do
    -- TODO Proper implementation of this method may require
    -- fundemental rethinking of our database scheme and rewriting many code.
    -- Should be done soon though.
    _ <- throwM err501
    execute queryText (Only student)
  where
    queryText :: Query
    queryText = [q|
        delete
        from      Students
        where     addr = ?
    |]

educatorGetStudents
    :: MonadEducatorAPIQuery m
    => Maybe Course -> m [StudentInfo]
educatorGetStudents courseF = do
    map (StudentInfo . fromOnly) <$> query queryText paramF
  where
    (clauseF, paramF) = mkFilterOn "course_id" courseF

    queryText = [q|
        select    addr
        from      Students
        left join StudentCourses
               on Students.addr = StudentCourses.student_addr
        where     1 = 1
    |]
      `filterClauses` [clauseF]

educatorGetCourses :: DBM m => Maybe Student -> m [CourseEducatorInfo]
educatorGetCourses studentF = do
    res :: [(Course, Text, Maybe Subject)] <- query queryText paramF
    return $
        -- group "subject" fields
        [ CourseEducatorInfo{..}
        | course@((ciId, ciDesc, _) : _) <- groupBy ((==) `on` view _1) res
        , let ciSubjects = course ^.. traversed . _3 . _Just
        ]
  where
    (clauseF, paramF) = mkFilterOn "student_addr" studentF

    queryText = [q|
        select    Courses.id, Courses.desc, Subjects.id
        from      Courses
        left join Subjects
               on Courses.id = Subjects.course_id
        left join StudentCourses
               on StudentCourses.course_id = Courses.id
        where     1 = 1
    |]
      `filterClauses` [clauseF]

educatorUnassignFromStudent
    :: MonadEducatorAPIQuery m
    => Student
    -> Hash Assignment
    -> m ()
educatorUnassignFromStudent student assignH = do
    -- we are not deleting other info since educator may want it to be preserved
    -- in case if he wants to assign as assignment again
    execute queryText (student, assignH)
  where
    queryText :: Query
    queryText = [q|
        delete
        from      StudentsAssignments
        where     student_addr = ?
              and assignment_hash = ?
    |]

isGradedSubmission
    :: MonadEducatorAPIQuery m
    => Hash Submission -> m Bool
isGradedSubmission submissionH = do
    checkExists queryText (Only submissionH)
  where
    queryText :: Query
    queryText = [q|
        select    count(*)
        from      Transactions
        where     submission_hash = ?
    |]

educatorGetGrades
    :: MonadEducatorAPIQuery m
    => Maybe Student
    -> Maybe Course
    -> Maybe IsFinal
    -> m [GradeInfo]
educatorGetGrades studentF courseIdF isFinalF = do
    query queryText (mconcat paramsF)
  where
    studentFilter = mkFilterOn "S.student_addr" studentF
    courseFilter = mkFilterOn "course_id" courseIdF
    assignTypeF = isFinalF ^. mapping (from assignmentTypeRaw)
    assignTypeFilter = mkFilterOn "A.type" assignTypeF
    (clausesF, paramsF) =
        unzip [studentFilter, courseFilter, assignTypeFilter]

    queryText = [q|
        select    T.grade, T.time, T.submission_hash, T.idx
        from      Submissions as S
        left join Assignments as A
               on A.hash = S.assignment_hash
        where     1 = 1
    |]
      `filterClauses` clausesF

educatorPostGrade
    :: MonadEducatorAPIQuery m
    => Hash Submission -> Grade -> m ()
educatorPostGrade subH grade = do
    time <- liftIO getCurrentTime
    transaction $ do
        sigSub <- getSignedSubmission subH
            `assertJust` AbsentError (SubmissionDomain subH)

        let ptx = PrivateTx
                { _ptSignedSubmission = sigSub
                , _ptTime = time
                , _ptGrade = grade
                }
            txId = getId ptx

        execute queryText
            ( txId
            , subH
            , grade
            , time
            , TxInMempool
            )
            `ifAlreadyExistsThrow` TransactionDomain txId
  where
    queryText :: Query
    queryText = [q|
        insert into  Transactions
        values       (?, ?, ?, ?, ?)
    |]

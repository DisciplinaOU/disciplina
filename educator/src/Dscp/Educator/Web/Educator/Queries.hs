{-# LANGUAGE QuasiQuotes #-}

module Dscp.Educator.Web.Educator.Queries
    ( educatorRemoveStudent
    , educatorGetStudents
    , educatorGetCourses
    , educatorGetCourse
    , educatorUnassignFromStudent
    , isGradedSubmission
    , educatorGetGrades
    , educatorPostGrade
    ) where

import Control.Lens (from, traversed)
import Data.List (groupBy)
import Data.Time.Clock (getCurrentTime)
import Servant (err501)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQLite
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Types
import Dscp.Util

es :: DatabaseSettings be EducatorSchema
es = educatorSchema

educatorRemoveStudent
    :: MonadEducatorWebQuery m
    => Student -> DBT t 'Writing m ()
educatorRemoveStudent student = do
    -- TODO [DSCP-176]: Proper implementation of this method may require
    -- fundemental rethinking of our database scheme and rewriting many code.
    -- Should be done soon though.
    _ <- throwM err501
    deleteByPk (esStudents es) student

educatorGetStudents
    :: MonadEducatorWebQuery m
    => Maybe Course -> DBT t w m [StudentInfo]
educatorGetStudents courseF = do
    runSelectMap StudentInfo . select $ do
        student <- all_ (esStudents es)
        whenJust courseF $ \course -> do
            studentId :-: courseId <- all_ (esStudentCourses es)
            guard_ (studentId ==. pk student)
            guard_ (courseId ==. valPk_ course)
        return (srAddr student)

educatorGetCourses
    :: DBM m
    => Maybe Student -> DBT t w m [CourseEducatorInfo]
educatorGetCourses studentF = do
    res :: [(Course, Text, Subject)] <-
        runSelect . select $ do
            subject <- all_ (esSubjects es)
            course <- related_ (esCourses es) (srCourse subject)

            whenJust studentF $ \student -> do
                studentId :-: courseId <- all_ (esStudentCourses es)
                guard_ (courseId `references_` course)
                guard_ (studentId ==. valPk_ student)

            return (crId course, crDesc course, srId subject)
    return
        -- group "subject" fields for the same courses
        [ CourseEducatorInfo{ ciId, ciDesc, ciSubjects }
        | course@((ciId, ciDesc, _) : _) <- groupBy ((==) `on` view _1) res
        , let ciSubjects = course ^.. traversed . _3
        ]

educatorGetCourse
    :: MonadEducatorWebQuery m
    => Course -> DBT t w m CourseEducatorInfo
educatorGetCourse courseId = do
    ciDesc <- selectByPk crDesc (esCourses es) courseId
                `assertJust` AbsentError (CourseDomain courseId)
    ciSubjects <- getCourseSubjects courseId
    return CourseEducatorInfo{ ciId = courseId, .. }

educatorUnassignFromStudent
    :: MonadEducatorWebQuery m
    => Student
    -> Hash Assignment
    -> DBT t 'Writing m ()
educatorUnassignFromStudent student assignH = do
    runDelete $ delete (esStudentAssignments es) (val_ (student <:-:> assignH) ==.)
    -- we are not deleting any other info since educator may want it to be preserved
    -- in case if he wants to assign an assignment again

-- TODO: Move to common?
isGradedSubmission
    :: MonadEducatorWebQuery m
    => Hash Submission -> DBT t w m Bool
isGradedSubmission submissionH =
    checkExists $ do
        privateTx <- all_ (esTransactions es)
        guard_ (trSubmissionHash privateTx ==. valPk_ submissionH)

educatorGetGrades
    :: MonadEducatorWebQuery m
    => Maybe Course
    -> Maybe Student
    -> Maybe (Hash Assignment)
    -> Maybe IsFinal
    -> DBT t w m [GradeInfo]
educatorGetGrades courseIdF studentF assignmentF isFinalF =
    runSelectMap gradeInfoFromRow . select $ do
        privateTx <- all_ (esTransactions es)
        submission <- related_ (esSubmissions es) (trSubmissionHash privateTx)
        assignment <- related_ (esAssignments es) (srAssignmentHash submission)

        whenJust courseIdF $ \courseId ->
            guard_ (arCourse assignment ==. valPk_ courseId)
        whenJust studentF $ \student ->
            guard_ (srStudent submission ==. valPk_ student)
        whenJust assignmentF $ \assignmentHash ->
            guard_ (valPk_ assignmentHash `references_` assignment)
        whenJust isFinalF $ \isFinal -> do
            let assignTypeF = isFinal ^. from assignmentTypeRaw
            guard_ (arType assignment ==. val_ assignTypeF)

        let TransactionRow{..} = privateTx
        let SubmissionRowId subHash = trSubmissionHash
        return (trGrade, trCreationTime, subHash, trIdx)

educatorPostGrade
    :: MonadEducatorWebQuery m
    => Hash Submission -> Grade -> DBT t 'Writing m ()
educatorPostGrade subH grade = do
    time <- liftIO getCurrentTime
    sigSub <- getSignedSubmission subH
        `assertJust` AbsentError (SubmissionDomain subH)

    let ptx = PrivateTx
            { _ptSignedSubmission = sigSub
            , _ptTime = time
            , _ptGrade = grade
            }
        txId = getId ptx

    rewrapAlreadyExists (TransactionDomain txId) $
        runInsert . insert (esTransactions es) . insertValue $
            TransactionRow
            { trHash = txId
            , trGrade = grade
            , trCreationTime = time
            , trIdx = TxInMempool
            , trSubmissionHash = packPk subH
            }

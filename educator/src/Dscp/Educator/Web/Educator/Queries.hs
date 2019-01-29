{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes    #-}

module Dscp.Educator.Web.Educator.Queries
    ( EducatorGetAssignmentsFilters (..)
    , EducatorGetSubmissionsFilters (..)
    , educatorRemoveStudent
    , educatorGetStudents
    , educatorGetCourses
    , educatorGetCourse
    , educatorUnassignFromStudent
    , educatorGetAssignment
    , educatorGetAssignments
    , educatorGetSubmission
    , educatorGetSubmissions
    , educatorGetGrades
    , educatorPostGrade
    ) where

import Control.Lens (from, mapping, traversed, _Just)
import Data.Default (Default)
import Data.List (groupBy)
import Data.Time.Clock (getCurrentTime)
import Servant (err501)
import Servant.Util (PaginationSpec)
import Servant.Util.Beam.Postgres (paginate_)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQL
import Dscp.Educator.DB
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Types
import Dscp.Util

----------------------------------------------------------------------------
-- Filters for endpoints
----------------------------------------------------------------------------

data EducatorGetAssignmentsFilters = EducatorGetAssignmentsFilters
    { afCourse  :: Maybe Course
    , afStudent :: Maybe Student
    , afDocType :: Maybe DocumentType
    , afIsFinal :: Maybe IsFinal
    } deriving (Show, Generic)

deriving instance Default EducatorGetAssignmentsFilters

data EducatorGetSubmissionsFilters = EducatorGetSubmissionsFilters
    { sfCourse         :: Maybe Course
    , sfStudent        :: Maybe Student
    , sfDocType        :: Maybe DocumentType
    , sfAssignmentHash :: Maybe $ Hash Assignment
    } deriving (Show, Generic)

deriving instance Default EducatorGetSubmissionsFilters

----------------------------------------------------------------------------
-- DB endpoints
----------------------------------------------------------------------------

es :: DatabaseSettings be EducatorSchema
es = educatorSchema

educatorRemoveStudent
    :: MonadEducatorWebQuery m
    => Student -> DBT t m ()
educatorRemoveStudent student = do
    -- TODO [DSCP-176]: Proper implementation of this method may require
    -- fundemental rethinking of our database scheme and rewriting many code.
    -- Should be done soon though.
    _ <- throwM err501
    void $ deleteByPk (esStudents es) student

educatorGetStudents
    :: MonadEducatorWebQuery m
    => Maybe Course -> PaginationSpec -> DBT t m [StudentInfo]
educatorGetStudents courseF pagination =
    runSelectMap StudentInfo . select $
    paginate_ pagination $ do
        student <- all_ (esStudents es)
        whenJust courseF $ \course ->
            link_ (esStudentCourses es) (pk_ student :-: valPk_ course)
        return (srAddr student)

educatorGetCourses
    :: DBM m
    => Maybe Student -> PaginationSpec -> DBT t m [CourseEducatorInfo]
educatorGetCourses studentF pagination = do
    res :: [(Course, ItemDesc, Maybe Subject)] <-
        runSelect . select $
        paginate_ pagination query
    return
        -- group "subject" fields for the same courses
        [ CourseEducatorInfo{ ciId, ciDesc, ciSubjects }
        | course@((ciId, ciDesc, _) : _) <- groupBy ((==) `on` view _1) res
        , let ciSubjects = course ^.. traversed . _3 . _Just
        ]
  where
    query = do
        course <- all_ (esCourses es)

        whenJust studentF $ \student ->
            link_ (esStudentCourses es) (valPk_ student :-: pk_ course)

        subject <- leftJoin_ (all_ $ esSubjects es)
                              ((`references_` course) . srCourse)
        return (crId course, crDesc course, srId subject)

educatorGetCourse
    :: MonadEducatorWebQuery m
    => Course -> DBT t m CourseEducatorInfo
educatorGetCourse courseId = do
    ciDesc <- selectByPk crDesc (esCourses es) courseId
                `assertJust` AbsentError (CourseDomain courseId)
    ciSubjects <- getCourseSubjects courseId
    return CourseEducatorInfo{ ciId = courseId, .. }

educatorUnassignFromStudent
    :: MonadEducatorWebQuery m
    => Student
    -> Hash Assignment
    -> DBT t m ()
educatorUnassignFromStudent student assignH = do
    changes <- runDelete $ delete
        (esStudentAssignments es)
        (==. val_ (student <:-:> assignH))

    unless (anyAffected changes) $
        throwM $ AbsentError (StudentAssignmentSubscriptionDomain student assignH)

    -- we are not deleting any other info since educator may want it to be preserved
    -- in case if he wants to assign an assignment again

-- | Get exactly one assignment.
educatorGetAssignment
    :: MonadEducatorWebQuery m
    => Hash Assignment
    -> DBT t m AssignmentEducatorInfo
educatorGetAssignment assignH =
    selectByPk educatorAssignmentInfoFromRow (esAssignments es) assignH
        >>= nothingToThrow (AbsentError $ AssignmentDomain assignH)

-- | Get educator assignments.
educatorGetAssignments
    :: MonadEducatorWebQuery m
    => EducatorGetAssignmentsFilters
    -> PaginationSpec
    -> DBT t m [AssignmentEducatorInfo]
educatorGetAssignments filters pagination =
    runSelectMap educatorAssignmentInfoFromRow . select $
    paginate_ pagination $ do
        assignment <- all_ (esAssignments es)

        guard_ $ filterMatchesPk_ (afCourse filters) (arCourse assignment)
        guard_ $ filterMatches_ assignTypeF (arType assignment)
        whenJust (afDocType filters) $ \docType ->
            guard_ (eqDocTypeQ docType (arContentsHash assignment))
        whenJust (afStudent filters) $ \student -> do
            link_ (esStudentAssignments es) (valPk_ student :-: pk_ assignment)

        return assignment
  where
    assignTypeF = afIsFinal filters ^. mapping (from assignmentTypeRaw)

-- | Get exactly one submission.
educatorGetSubmission
    :: MonadEducatorWebQuery m
    => Hash Submission
    -> DBT t m SubmissionEducatorInfo
educatorGetSubmission subH = do
    submissions <- runSelectMap educatorSubmissionInfoFromRow . select $ do
        submission <- related_ (esSubmissions es) (valPk_ subH)
        mPrivateTx <- leftJoin_ (all_ $ esTransactions es)
                                ((`references_` submission) . trSubmission)
        return (submission, mPrivateTx)
    listToMaybeWarn submissions
        >>= nothingToThrow (AbsentError $ SubmissionDomain subH)

-- | Get all registered submissions.
educatorGetSubmissions
    :: MonadEducatorWebQuery m
    => EducatorGetSubmissionsFilters
    -> PaginationSpec
    -> DBT t m [SubmissionEducatorInfo]
educatorGetSubmissions filters pagination =
    runSelectMap educatorSubmissionInfoFromRow . select $
    paginate_ pagination $ do
        submission <- all_ (esSubmissions es)
        assignment <- related_ (esAssignments es) (srAssignment submission)

        guard_ $ filterMatchesPk_ (sfCourse filters) (arCourse assignment)
        guard_ $ filterMatchesPk_ (sfAssignmentHash filters) (srAssignment submission)
        whenJust (sfDocType filters) $ \docType ->
            guard_ (eqDocTypeQ docType (arContentsHash assignment))
        whenJust (sfStudent filters) $ \student -> do
            link_ (esStudentAssignments es) (valPk_ student :-: pk_ assignment)

        mPrivateTx <- leftJoin_ (all_ $ esTransactions es)
                                ((`references_` submission) . trSubmission)

        return (submission, mPrivateTx)

educatorGetGrades
    :: MonadEducatorWebQuery m
    => Maybe Course
    -> Maybe Student
    -> Maybe (Hash Assignment)
    -> Maybe IsFinal
    -> DBT t m [GradeInfo]
educatorGetGrades courseIdF studentF assignmentF isFinalF =
    runSelectMap gradeInfoFromRow . select $ do
        privateTx <- all_ (esTransactions es)
        submission <- related_ (esSubmissions es) (trSubmission privateTx)
        assignment <- related_ (esAssignments es) (srAssignment submission)

        guard_ $ filterMatchesPk_ courseIdF (arCourse assignment)
        guard_ $ filterMatchesPk_ studentF (srStudent submission)
        guard_ $ filterMatchesPk_ assignmentF (pk_ assignment)
        whenJust isFinalF $ \isFinal -> do
            let assignTypeF = isFinal ^. from assignmentTypeRaw
            guard_ (arType assignment ==. val_ assignTypeF)

        return privateTx

educatorPostGrade
    :: MonadEducatorWebQuery m
    => Hash Submission -> Grade -> DBT 'WithinTx m ()
educatorPostGrade subH grade = do
    time <- toTimestamp <$> liftIO getCurrentTime
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
            , trSubmission = packPk subH
            }

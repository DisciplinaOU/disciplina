{-# LANGUAGE DeriveAnyClass #-}

module Dscp.Educator.Web.Student.Queries
    ( StudentGetAssignmentsFilters (..)
    , StudentGetSubmissionsFilters (..)
    , studentIsCourseFinished
    , studentGetCourse
    , studentGetCourses
    , studentGetGrade
    , studentGetLastAssignmentSubmission
    , studentGetAssignment
    , studentGetAssignments
    , studentGetSubmission
    , studentGetSubmissions
    ) where

import Control.Lens (from, mapping)
import Data.Coerce (coerce)
import Data.Default (Default)
import Dscp.Core
import Dscp.Crypto (Hash)
import Dscp.DB.SQL
import Dscp.Educator.DB
import qualified Dscp.Educator.DB as Base
import Dscp.Util

import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Student.Types
import Dscp.Educator.Web.Types
import Dscp.Util.Servant

----------------------------------------------------------------------------
-- Filters for endpoints
----------------------------------------------------------------------------

data StudentGetAssignmentsFilters = StudentGetAssignmentsFilters
    { afCourse  :: Maybe Course
    , afDocType :: Maybe DocumentType
    , afIsFinal :: Maybe IsFinal
    } deriving (Show, Generic)

deriving instance Default StudentGetAssignmentsFilters

data StudentGetSubmissionsFilters = StudentGetSubmissionsFilters
    { sfCourse         :: Maybe Course
    , sfAssignmentHash :: Maybe $ Hash Assignment
    , sfDocType        :: Maybe DocumentType
    } deriving (Show, Generic)

deriving instance Default StudentGetSubmissionsFilters

----------------------------------------------------------------------------
-- DB endpoints
----------------------------------------------------------------------------

es :: DatabaseSettings be EducatorSchema
es = educatorSchema

studentIsCourseFinished
    :: MonadEducatorWebQuery m
    => Id Student -> Course -> DBT t m Bool
studentIsCourseFinished studentId' courseId' =
    checkExists $ do
        privateTx <- all_ (esTransactions es)
        submission <- related_ (esSubmissions es) (trSubmission privateTx)
        assignment <- related_ (esAssignments es) (srAssignment submission)
        guard_ (arType assignment ==. val_ CourseFinal)
        guard_ (srStudent submission ==. valPk_ studentId')
        guard_ (arCourse assignment ==. valPk_ courseId')
        guard_ (isPositiveGradeQ (trGrade privateTx))

studentGetCourse
    :: MonadEducatorWebQuery m
    => Id Student -> Course -> DBT 'WithinTx m CourseStudentInfo
studentGetCourse studentId courseId = do
    ciDesc <- selectByPk crDesc (esCourses es) courseId
        `assertJust` AbsentError (CourseDomain courseId)
    ciIsEnrolled <- Base.isEnrolledTo studentId courseId
    ciSubjects <- Base.getCourseSubjects courseId
    ciIsFinished <- studentIsCourseFinished studentId courseId
    return CourseStudentInfo{ ciId = courseId, .. }

studentGetCourses
    :: MonadEducatorWebQuery m
    => Id Student
    -> Maybe IsEnrolled
    -> SortingSpecOf CourseStudentInfo
    -> DBT 'WithinTx m [CourseStudentInfo]
studentGetCourses studentId (coerce -> isEnrolledF) sorting = do
    courses <- runSelect . select $
        orderBy_ (bySpec_ sorting . mkSortingSpecApp) $ do
            course <- all_ (esCourses es)
            whenJust isEnrolledF $ \isEnrolled -> do
                let isEnrolled' = exists_ $ do
                        link_ (esStudentCourses es) (valPk_ studentId :-: pk_ course)
                        return (as_ @Int 1)
                guard_ (val_ isEnrolled ==. isEnrolled')
            return (crId course, crDesc course)

    forM courses $ \(courseId, ciDesc) -> do
        ciIsEnrolled <- Base.isEnrolledTo studentId courseId
        ciSubjects <- Base.getCourseSubjects courseId
        ciIsFinished <- studentIsCourseFinished studentId courseId
        return CourseStudentInfo{ ciId = courseId, .. }
  where
    mkSortingSpecApp (courseId, desc) =
        fieldSort_ @"id" courseId .*.
        fieldSort_ @"desc" desc .*.
        HNil

studentGetGrade
    :: MonadEducatorWebQuery m
    => Hash Submission -> DBT 'WithinTx m (Maybe GradeInfo)
studentGetGrade submissionH = do
    mgrade <- listToMaybeWarnM . runSelect . select $ do
        ptx <- all_ (esTransactions es)
        let SubmissionRowId subH = trSubmission ptx
        guard_ (subH ==. val_ submissionH)
        return $ (trGrade ptx, trCreationTime ptx, subH, trIdx ptx)

    forM mgrade $ \(giGrade, giTimestamp, giSubmissionHash, blkIdx) -> do
        let giHasProof = blkIdx /= TxInMempool
        return GradeInfo{..}

studentGetLastAssignmentSubmission
    :: MonadEducatorWebQuery m
    => Student
    -> Hash Assignment
    -> DBT 'WithinTx m (Maybe SubmissionStudentInfo)
studentGetLastAssignmentSubmission student assignH = do
    msubmission <- listToMaybeWarnM . runSelect . select $ do
        submission <- limit_ 1 . orderBy_ (desc_ . srCreationTime) $
                      all_ (esSubmissions es)
        guard_ (srAssignment submission ==. valPk_ assignH)
        guard_ (srStudent submission ==. valPk_ student)
        let AssignmentRowId assignHash = srAssignment submission
        return (srHash submission, srContentsHash submission, assignHash)

    forM msubmission $ \(submissionH, siContentsHash, siAssignmentHash) -> do
        siGrade <- studentGetGrade submissionH
        return SubmissionStudentInfo{ siHash = submissionH, .. }

-- | Helper to post-process fetched assignment and full it with missing info.
studentComplementAssignmentInfo
    :: MonadEducatorWebQuery m
    => Student
    -> Hash Assignment
    -> Assignment
    -> DBT 'WithinTx m AssignmentStudentInfo
studentComplementAssignmentInfo student assignH assignment = do
    aiLastSubmission <- studentGetLastAssignmentSubmission student assignH
    return AssignmentStudentInfo
        { aiHash = assignH
        , aiCourseId = _aCourseId assignment
        , aiContentsHash = _aContentsHash assignment
        , aiDesc = _aDesc assignment
        , aiIsFinal = _aType assignment ^. assignmentTypeRaw
        , ..
        }

-- | Get exactly one assignment.
studentGetAssignment
    :: MonadEducatorWebQuery m
    => Student
    -> Hash Assignment
    -> DBT 'WithinTx m AssignmentStudentInfo
studentGetAssignment student assignH = do
    assignments <- runSelectMap assignmentFromRow . select $ do
        assignment <- related_ (esAssignments es) (valPk_ assignH)
        link_ (esStudentAssignments es) (valPk_ student :-: pk_ assignment)
        return assignment
    assignment <-
        listToMaybeWarn assignments
        >>= nothingToThrow (AbsentError $ AssignmentDomain assignH)
    studentComplementAssignmentInfo student assignH assignment


-- | Get student assignments.
studentGetAssignments
    :: MonadEducatorWebQuery m
    => Student
    -> StudentGetAssignmentsFilters
    -> SortingSpecOf AssignmentStudentInfo
    -> DBT 'WithinTx m [AssignmentStudentInfo]
studentGetAssignments student filters sorting = do
    assignments <- runSelectMap (arHash &&& assignmentFromRow) . select $ do
        assignment <-
            orderBy_ (bySpec_ sorting . mkSortingSpecApp) $
            all_ (esAssignments es)

        link_ (esStudentAssignments es) (valPk_ student :-: pk_ assignment)

        guard_ $ filterMatchesPk_ (afCourse filters) (arCourse assignment)
        guard_ $ filterMatches_ assignTypeF (arType assignment)
        whenJust (afDocType filters) $ \docType ->
            guard_ (eqDocTypeQ docType (arContentsHash assignment))

        return assignment

    forM assignments $ \(assignH, assignment) ->
        studentComplementAssignmentInfo student assignH assignment
  where
    assignTypeF = afIsFinal filters ^. mapping (from assignmentTypeRaw)
    mkSortingSpecApp AssignmentRow{..} =
        fieldSort_ @"course" (unpackPk arCourse) .*.
        fieldSort_ @"desc" arDesc .*.
        HNil

-- | Get exactly one assignment.
studentGetSubmission
    :: MonadEducatorWebQuery m
    => Student
    -> Hash Submission
    -> DBT t m SubmissionStudentInfo
studentGetSubmission student subH = do
    submissions <- runSelectMap studentSubmissionInfoFromRow . select $ do
        submission <- related_ (esSubmissions es) (valPk_ subH)
        assignment <- related_ (esAssignments es) (srAssignment submission)
        link_ (esStudentAssignments es) (valPk_ student :-: pk_ assignment)

        mPrivateTx <- leftJoin_ (all_ $ esTransactions es)
                                ((`references_` submission) . trSubmission)

        return (submission, mPrivateTx)
    listToMaybeWarn submissions
        >>= nothingToThrow (AbsentError $ SubmissionDomain subH)

-- | Get student submissions.
studentGetSubmissions
    :: MonadEducatorWebQuery m
    => Student
    -> StudentGetSubmissionsFilters
    -> SortingSpecOf SubmissionStudentInfo
    -> DBT t m [SubmissionStudentInfo]
studentGetSubmissions student filters sorting = do
    runSelectMap studentSubmissionInfoFromRow . select $
        orderBy_ (bySpec_ sorting . mkSortingSpecApp) $ do
            submission <- all_ (esSubmissions es)

            assignment <- related_ (esAssignments es) (srAssignment submission)
            link_ (esStudentAssignments es) (valPk_ student :-: pk_ assignment)

            guard_ $ filterMatchesPk_ (sfCourse filters) (arCourse assignment)
            guard_ $ filterMatchesPk_ (sfAssignmentHash filters) (srAssignment submission)
            whenJust (sfDocType filters) $ \docType ->
                guard_ (eqDocTypeQ docType (arContentsHash assignment))

            mPrivateTx <- leftJoin_ (all_ $ esTransactions es)
                                    ((`references_` submission) . trSubmission)

            return (submission, mPrivateTx)
  where
    mkSortingSpecApp (_, TransactionRow{..}) =
        fieldSort_ @"grade" trGrade .*.
        HNil

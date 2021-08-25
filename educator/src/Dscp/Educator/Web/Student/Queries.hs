{-# LANGUAGE DeriveAnyClass #-}

module Dscp.Educator.Web.Student.Queries
    ( studentIsCourseFinished
    , studentGetCourse
    , studentGetCourses
    , studentGetGrade
    , studentGetLastAssignmentSubmission
    , studentGetAssignment
    , studentGetAssignments
    , studentGetSubmission
    , studentGetSubmissions
    ) where

import Control.Lens (from)
import Servant.Util (FilteringSpecOf, HList (HNil), PaginationSpec, SortingSpecOf, (.*.))
import Servant.Util.Beam.Postgres (fieldSort, filterOn, manualFilter, matches_, paginate_, sortBy_)

import Dscp.Core
import Dscp.Crypto (Hash)
import Dscp.DB.SQL
import Dscp.Educator.DB
import qualified Dscp.Educator.DB as Base
import Dscp.Util

import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Student.Types
import Dscp.Educator.Web.Types

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
    -> FilteringSpecOf CourseStudentInfo
    -> SortingSpecOf CourseStudentInfo
    -> PaginationSpec
    -> DBT 'WithinTx m [CourseStudentInfo]
studentGetCourses studentId filtering sorting pagination = do
    courses <- runSelect . select $
        paginate_ pagination $
        sortBy_ sorting mkSortingSpecApp $ do
            course <- all_ (esCourses es)
            guard_ $ matches_ filtering (mkFilteringSpecApp course)
            return (crId course, crDesc course)

    forM courses $ \(courseId, ciDesc) -> do
        ciIsEnrolled <- Base.isEnrolledTo studentId courseId
        ciSubjects <- Base.getCourseSubjects courseId
        ciIsFinished <- studentIsCourseFinished studentId courseId
        return CourseStudentInfo{ ciId = courseId, .. }
  where
    mkFilteringSpecApp course =
        manualFilter (isEnrolledF course) .*.
        filterOn (crDesc course) .*.
        HNil
    mkSortingSpecApp (courseId, desc) =
        fieldSort @"id" courseId .*.
        fieldSort @"desc" desc .*.
        HNil

    isEnrolledF course (IsEnrolled isEnrolled) =
        let isEnrolled' = existsAny_ $
                link_ (esStudentCourses es) (valPk_ studentId :-: pk_ course)
        in val_ isEnrolled ==. isEnrolled'

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
    -> FilteringSpecOf AssignmentStudentInfo
    -> SortingSpecOf AssignmentStudentInfo
    -> PaginationSpec
    -> DBT 'WithinTx m [AssignmentStudentInfo]
studentGetAssignments student filters sorting pagination = do
    assignments <- runSelectMap (arHash &&& assignmentFromRow) . select $
        paginate_ pagination $ do
            assignment <-
                sortBy_ sorting mkSortingSpecApp $
                all_ (esAssignments es)

            link_ (esStudentAssignments es) (valPk_ student :-: pk_ assignment)

            guard_ $ matches_ filters (mkFilteringSpecApp assignment)

            return assignment

    forM assignments $ \(assignH, assignment) ->
        studentComplementAssignmentInfo student assignH assignment
  where
    mkFilteringSpecApp AssignmentRow{..} =
        filterOn (unpackPk arCourse) .*.
        manualFilter (`eqDocTypeQ` arContentsHash) .*.
        manualFilter (\isFinal -> val_ (isFinal ^. from assignmentTypeRaw) ==. arType) .*.
        filterOn arDesc .*.
        HNil
    mkSortingSpecApp AssignmentRow{..} =
        fieldSort @"course" (unpackPk arCourse) .*.
        fieldSort @"desc" arDesc .*.
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
    -> FilteringSpecOf SubmissionStudentInfo
    -> SortingSpecOf SubmissionStudentInfo
    -> PaginationSpec
    -> DBT t m [SubmissionStudentInfo]
studentGetSubmissions student filters sorting pagination = do
    runSelectMap studentSubmissionInfoFromRow . select $
        paginate_ pagination $
        sortBy_ sorting mkSortingSpecApp $ do
            submission <- all_ (esSubmissions es)
            assignment <- related_ (esAssignments es) (srAssignment submission)
            link_ (esStudentAssignments es) (valPk_ student :-: pk_ assignment)

            guard_ $ matches_ filters $
                mkFilteringSpecApp (submission, assignment)

            mPrivateTx <- leftJoin_ (all_ $ esTransactions es)
                                    ((`references_` submission) . trSubmission)
            return (submission, mPrivateTx)
  where
    mkFilteringSpecApp (SubmissionRow{..}, AssignmentRow{..}) =
        filterOn (unpackPk arCourse) .*.
        filterOn arHash .*.
        manualFilter (`eqDocTypeQ` arContentsHash) .*.
        HNil
    mkSortingSpecApp (_, TransactionRow{..}) =
        fieldSort @"grade" trGrade .*.
        HNil

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Dscp.Educator.Web.Student.Queries
    ( studentIsCourseFinished
    , studentGetCourse
    , studentGetCourses
    , studentGetGrade
    , studentGetLastAssignmentSubmission
    ) where

import Data.Coerce (coerce)
import Database.SQLite.Simple (Only (..), Query)
import Text.InterpolatedString.Perl6 (q)

import Dscp.Core
import Dscp.Crypto (Hash)
import Dscp.DB.SQLite
import qualified Dscp.DB.SQLite.Queries as Base
import Dscp.Util

import Dscp.Educator.Web.Student.Types
import Dscp.Educator.Web.Types

es :: DatabaseSettings be EducatorSchema
es = educatorSchema

-- | Check whether a corresponding assignment is successfully passed.
-- TODO: move somewhere
isPositiveGradeQuery
    :: _
    => QGenExpr context syntax s Grade -> QGenExpr context syntax s Bool
isPositiveGradeQuery = (>=. val_ (UnsafeGrade 0))

studentIsCourseFinished
    :: MonadEducatorWebQuery m
    => Id Student -> Course -> DBT t w m Bool
studentIsCourseFinished studentId' courseId' =
    checkExists $ do
        privateTx <- all_ (esTransactions es)
        submission <- related_ (esSubmissions es) (trSubmissionHash privateTx)
        assignment <- related_ (esAssignments es) (srAssignmentHash submission)
        guard_ (arType assignment ==. val_ CourseFinal)
        guard_ (srStudent submission ==. valPk_ studentId')
        guard_ (arCourse assignment ==. valPk_ courseId')
        guard_ (isPositiveGradeQuery (trGrade privateTx))

studentGetCourse
    :: MonadEducatorWebQuery m
    => Id Student -> Course -> DBT 'WithinTx w m CourseStudentInfo
studentGetCourse studentId courseId = do
    ciDesc <- selectByPk crDesc (esCourses es) courseId
        `assertJust` AbsentError (CourseDomain courseId)
    ciIsEnrolled <- Base.isEnrolledTo studentId courseId
    ciSubjects <- Base.getCourseSubjects courseId
    ciIsFinished <- studentIsCourseFinished studentId courseId
    return CourseStudentInfo{ ciId = courseId, .. }

studentGetCourses
    :: MonadEducatorWebQuery m
    => Id Student -> Maybe IsEnrolled -> DBT 'WithinTx w m [CourseStudentInfo]
studentGetCourses studentId (coerce -> isEnrolledF) = do
    courses <- runSelect . select $ do
        course <- all_ (esCourses es)
        whenJust isEnrolledF $ \isEnrolled -> do
            let isEnrolled' = exists_ $ do
                    studentId' :-: CourseRowId courseId' <- all_ (esStudentCourses es)
                    guard_ (courseId' ==. crId course)
                    guard_ (studentId' ==. valPk_ studentId)
                    return studentId'
            guard_ (val_ isEnrolled ==. isEnrolled')
        return (crId course, crDesc course)

    forM courses $ \(courseId, ciDesc) -> do
        ciIsEnrolled <- Base.isEnrolledTo studentId courseId
        ciSubjects <- Base.getCourseSubjects courseId
        ciIsFinished <- studentIsCourseFinished studentId courseId
        return CourseStudentInfo{ ciId = courseId, .. }

studentGetGrade
    :: MonadEducatorWebQuery m
    => Hash Submission -> DBT 'WithinTx w m (Maybe GradeInfo)
studentGetGrade submissionH = do
    mgrade <- listToMaybeWarnM . runSelect . select $ do
        ptx <- all_ (esTransactions es)
        let SubmissionRowId subH = trSubmissionHash ptx
        guard_ (subH ==. val_ submissionH)
        return $ (trGrade ptx, trCreationTime ptx, subH, trIdx ptx)

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
    :: MonadEducatorWebQuery m
    => Student
    -> Hash Assignment
    -> DBT 'WithinTx w m (Maybe SubmissionStudentInfo)
studentGetLastAssignmentSubmission student assignH = do
    msubmission <- listToMaybeWarnM . runSelect . select $ do
        submission <- limit_ 1 . orderBy_ (desc_ . srCreationTime) $
                      all_ (esSubmissions es)
        guard_ (srAssignmentHash submission ==. valPk_ assignH)
        guard_ (srStudent submission ==. valPk_ student)
        let AssignmentRowId assignHash = srAssignmentHash submission
        return (srHash submission, srContentsHash submission, assignHash)

    forM msubmission $ \(submissionH, siContentsHash, siAssignmentHash) -> do
        siGrade <- studentGetGrade submissionH
        return SubmissionStudentInfo{ siHash = submissionH, .. }

-- | Student API handlers

module Dscp.Educator.Web.Student.Handlers
       ( studentApiHandlers
       , oneGeek
       , oneGeekSK
       ) where

import Dscp.Core (Student)
import Dscp.Core.Arbitrary (studentEx, studentSKEx)
import Dscp.Crypto
import Dscp.DB.SQLite (sqlTransaction)
import Dscp.Educator.Web.Student.API
import Dscp.Educator.Web.Student.Logic
import Dscp.Educator.Web.Student.Queries

type StudentApiWorkMode m =
    ( MonadStudentAPIQuery m
    )

-- TODO [DSCP-141]: remove these two
oneGeek :: Student
oneGeek = studentEx

oneGeekSK :: SecretKey
oneGeekSK = studentSKEx

studentApiHandlers
    :: forall m. StudentApiWorkMode m
    => StudentApiHandlers m
studentApiHandlers =
    StudentApiEndpoints
    { sGetCourses = \isEnrolledF ->
        sqlTransaction $ studentGetCourses oneGeek isEnrolledF

    , sGetCourse = \course ->
        studentGetCourse oneGeek course

    , sGetAssignments = \courseIdF docTypeF isFinalF ->
        sqlTransaction $ studentGetAssignments oneGeek courseIdF docTypeF isFinalF

    , sGetAssignment = \assignH ->
        sqlTransaction $ studentGetAssignment oneGeek assignH

    , sGetSubmissions = \courseIdF assignHF docTypeF ->
        sqlTransaction $ studentGetSubmissions oneGeek courseIdF assignHF docTypeF

    , sGetSubmission = \subH ->
        sqlTransaction $ studentGetSubmission oneGeek subH

    , sMakeSubmission = \newSub ->
        studentMakeSubmissionVerified oneGeek newSub

    , sDeleteSubmission = \subH ->
        sqlTransaction $ studentDeleteSubmission oneGeek subH

    , sGetProofs = \sinceF ->
        sqlTransaction $ studentGetProofs oneGeek sinceF
    }

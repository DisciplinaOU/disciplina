{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Educator HTTP API definition.

module Dscp.Educator.Web.Educator.API
    ( EducatorApiEndpoints (..)
    , EducatorAPI
    , ProtectedEducatorAPI
    , educatorAPI
    , protectedEducatorAPI
    , EducatorApiHandlers
    ) where

import Servant
import Servant.Generic

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Educator.Auth
import Dscp.Educator.Web.Educator.Error
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Types

type EducatorAPI =
    "api" :> "educator" :> "v1" :> ToServant (EducatorApiEndpoints AsApi)

type ProtectedEducatorAPI =
    Auth' [EducatorAuth, NoAuth "educator"] () :> EducatorAPI

type EducatorApiHandlers m = EducatorApiEndpoints (AsServerT m)

educatorAPI :: Proxy EducatorAPI
educatorAPI = Proxy

protectedEducatorAPI :: Proxy ProtectedEducatorAPI
protectedEducatorAPI = Proxy

-- TODO [DSCP-176]: add a way to fetch ALL assignments, even whose which are not assigned to any student

data EducatorApiEndpoints route = EducatorApiEndpoints
    {
      -- Students

      eNewStudent :: route
        :- "students"
        :> Summary "Add a new student address to a database"
        :> ReqBody '[DSON] NewStudent
        :> PostCreated '[DSON] ()

    , eRemoveStudent :: route
        :- "students" :> Capture "studentAddr" Student
        :> Summary "Remove a student from a database"
        :> Description "Removes a student only if he's not currently \
                        \attending any course. We will not automatically \
                        \perform a cascade deletion, because it will make \
                        \this operation particularly dangerous. If a student \
                        \attends any course, an error will be raised."
        :> Delete '[DSON] ()

    , eGetStudents :: route
        :- "students"
        :> QueryParam "course" Course
        :> Summary "Get a list of all registered students' addresses"
        :> Get '[DSON] [StudentInfo]

      -- Courses

    , eAddCourse :: route
        :- "courses"
        :> Summary "Add a new course to a database"
        :> ReqBody '[DSON] NewCourse
        :> PostCreated '[DSON] Course
    -- TODO: return proper JSON-object here

    , eGetCourses :: route
        :- "courses"
        :> QueryParam "student" Student
        :> Summary "Get all courses"
        :> Get '[DSON] [CourseEducatorInfo]

    , eEnrollStudentToCourse :: route
        :- "students" :> Capture "studentAddr" Student
        :> "courses"
        :> Summary "Enroll a student in a new course"
        :> Description "Given existing student and course, enroll the \
                        \student to the course."
        :> ReqBody '[DSON] EnrollStudentToCourse
        :> PostCreated '[DSON] ()

      -- Assignments

    , eAddCourseAssignment :: route
        :- "assignments"
        :> Summary "Add assignment to a course"
        :> QueryFlag "autoAssign"
        :> ReqBody '[DSON] NewAssignment
        :> PostCreated '[DSON] ()

    , eGetAssignments :: route
        :- "assignments"
        :> QueryParam "course" Course
        :> QueryParam "student" Student
        :> QueryParam "isFinal" IsFinal
        :> Summary "Get all assignments"
        :> Get '[DSON] [AssignmentEducatorInfo]

    , eAssignToStudent :: route
        :- "students" :> Capture "studentAddr" Student
        :> "assignments"
        :> Summary "Assign an assignment to a student"
        :> Description "Assigns a new assignment to a student in scope of \
                        \given course."
        :> ReqBody '[DSON] AssignToStudent
        :> PostCreated '[DSON] ()

    , eUnassignFromStudent :: route
        :- "students"    :> Capture "studentAddr" Student
        :> "assignments" :> Capture "assignmentHash" (Hash Assignment)
        :> Summary "Unassign an assignment from a student"
        :> Description "If given student has been assigned a given \
                        \assignment, then unassigns it from them, otherwise \
                        \raises error."
        :> Delete '[DSON] ()

      -- Submissions

    , eGetSubmission :: route
        :- "submissions" :> Capture "submissionHash" (Hash Submission)
        :> Summary "Get info about a submission"
        :> Description "Gets a submission data by given submission hash."
        :> Get '[DSON] SubmissionEducatorInfo

    , eDeleteSubmission :: route
        :- "submissions" :> Capture "submissionHash" (Hash Submission)
        :> Summary "Delete a submission"
        :> Description "Deletes a submission from a database. Only ungraded \
                        \submissions can be deleted."
        :> Delete '[DSON] ()

    , eGetSubmissions :: route
        :- "submissions"
        :> QueryParam "course" Course
        :> QueryParam "student" Student
        :> QueryParam "assignment" (Hash Assignment)
        :> Summary "Get all submissions"
        :> Description "Gets a list of all submissions done by all students. \
                      \This method is inaccessible by students."
        :> Get '[DSON] [SubmissionEducatorInfo]

      -- Grades

    , ePostGrade :: route
        :- "grades"
        :> Summary "Post a new grade"
        :> Description "Posts a new grade with a given body."
        :> ReqBody '[DSON] NewGrade
        :> PostCreated '[DSON] ()

    , eGetGrades :: route
        :- "grades"
        :> QueryParam "course" Course
        :> QueryParam "student" Student
        :> QueryParam "assignment" (Hash Assignment)
        :> QueryParam "isFinal" IsFinal
        :> Summary "Get all grades"
        :> Description "Gets a list of all grades performed by all students."
        :> Get '[DSON] [GradeInfo]

      -- Proofs

    , eGetProofs :: route
        :- "proofs"
        :> QueryParam "course" Course
        :> QueryParam "student" Student
        :> QueryParam "assignment" (Hash Assignment)
        :> Summary "Get proofs of all student's activity"
        :> Description "Gets all private transactions related to a student \
                        \together with corresponding Merkle proofs."
        :> Get '[DSON] [BlkProofInfo]

   } deriving (Generic)

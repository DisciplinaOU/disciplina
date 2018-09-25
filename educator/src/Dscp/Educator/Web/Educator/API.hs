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
        :> ReqBody '[DSON] Student
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
        :> Summary "Get a list of all registered students' addresses"
        :> QueryParam "courseId" Course
        :> Get '[DSON] [StudentInfo]

      -- Courses

    , eAddCourse :: route
        :- "courses"
        :> Summary "Add a new course to a database"
        :> ReqBody '[DSON] NewCourse
        :> PostCreated '[DSON] ()

    , eGetCourses :: route
        :- "courses"
        :> Summary "Get all courses"
        :> Get '[DSON] [CourseEducatorInfo]

    , eEnrollStudentToCourse :: route
        :- "students" :> Capture "studentAddr" Student
        :> "courses"
        :> Summary "Enroll a student in a new course"
        :> Description "Given existing student and course, enroll the \
                        \student to the course."
        :> ReqBody '[DSON] EnrollStudentToCourse
        :> Post '[DSON] ()

    , eGetStudentCourses :: route
        :- "students" :> Capture "studentAddr" Student
        :> "courses"
        :> Summary "Get a list of student's courses"
        :> Description "Gets a list of courses which student is currently \
                        \attending."
        :> Get '[DSON] [CourseEducatorInfo]

      -- Assignments

    , eAddCourseAssignment :: route
        :- "assignments"
        :> Summary "Add assignment to a course"
        :> QueryFlag "autoAssign"
        :> ReqBody '[DSON] NewAssignment
        :> PostCreated '[DSON] ()

    , eGetStudentAssignments :: route
        :- "students" :> Capture "studentAddr" Student
        :> "assignments"
        :> Summary "Get active student's assignments"
        :> Description "Given student address, gets a list of all pending \
                        \assignments student has."
        :> Get '[DSON] [AssignmentEducatorInfo]

    , eAssignToStudent :: route
        :- "students" :> Capture "studentAddr" Student
        :> "assignments"
        :> Summary "Assign an assignment to a student"
        :> Description "Assigns a new assignment to a student in scope of \
                        \given course."
        :> ReqBody '[DSON] (Hash Assignment)
        :> PostCreated '[DSON] ()

    , eUnassignFromStudent :: route
        :- "students"    :> Capture "studentAddr" Student
        :> "assignments" :> Capture "assignmentHash" (Hash Assignment)
        :> Summary "Unassign an assignment from a student"
        :> Description "If given student has been assigned a given \
                        \assignment, then unassigns it from them, otherwise \
                        \raises error."
        :> Delete '[DSON] ()

    , eGetStudentCourseAssignments :: route
        :- "students" :> Capture "studentAddr" Student
        :> "courses"  :> Capture "courseId" Course
        :> "assignments"
        :> Summary "Get active student's assignments for a given course"
        :> Description "Given student address and course ID, gets a list of \
                        \all pending assignments student has as a part of a \
                        \course."
        :> QueryParam "isFinal" IsFinal
        :> Get '[DSON] [AssignmentEducatorInfo]

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
        :> Summary "Get all submissions"
        :> Description "Gets a list of all submissions done by all students. \
                      \This method is inaccessible by students."
        :> Get '[DSON] [SubmissionEducatorInfo]

    , eGetStudentSubmissions :: route
        :- "students" :> Capture "studentAddr" Student
        :> "submissions"
        :> Summary "Get all student's submissions"
        :> Description "Gets a list of all student's submissions."
        :> Get '[DSON] [SubmissionEducatorInfo]

    , eGetStudentAssignmentSubmissions :: route
        :- "students"    :> Capture "studentAddr" Student
        :> "assignments" :> Capture "assignmentHash" (Hash Assignment)
        :> "submissions"
        :> Summary "Student's submissions for an assignment"
        :> Description "Gets a list of student's submissions for a given \
                    \assignment"
        :> Get '[DSON] [SubmissionEducatorInfo]

    , eGetStudentCourseSubmissions :: route
        :- "students" :> Capture "studentAddr" Student
        :> "courses"  :> Capture "courseId" Course
        :> "submissions"
        :> Summary "Get student's course submissions"
        :> Description "Gets a list of student's submissions he made during \
                        \studying given course."
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
        :> Summary "Get all grades"
        :> Description "Gets a list of all grades performed by all students."
        :> Get '[DSON] [GradeInfo]

    , eGetStudentGrades :: route
        :- "students" :> Capture "studentAddr" Student
        :> "grades"
        :> Summary "Get all student's grades"
        :> Description "Gets a list of all students grades (aka transactions \
                        \in a private chain)"
        :> Get '[DSON] [GradeInfo]

    , eGetStudentCourseGrades :: route
        :- "students" :> Capture "studentAddr" Student
        :> "courses"  :> Capture "courseId" Course
        :> "grades"
        :> Summary "Get student's course grades"
        :> Description "Gets a list of grades a student received during \
                        \studying given course."
        :> QueryParam "isFinal" IsFinal
        :> Get '[DSON] [GradeInfo]

      -- Proofs

    , eGetStudentProofs :: route
        :- "students" :> Capture "studentAddr" Student
        :> "proofs"
        :> Summary "Get proofs of all student's activity"
        :> Description "Gets all private transactions related to a student \
                        \together with corresponding Merkle proofs."
        :> Get '[DSON] [BlkProofInfo]

    , eGetStudentCourseProofs :: route
        :- "students" :> Capture "studentAddr" Student
        :> "courses"  :> Capture "courseId" Course
        :> "proofs"
        :> Summary "Get proofs of student's course progress"
        :> Description "Gets student's course grades and submissions in form \
                        \of private transactions, together with corresponding \
                        \Merkle proofs."
        :> Get '[DSON] [BlkProofInfo]

   } deriving (Generic)

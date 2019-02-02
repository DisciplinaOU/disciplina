-- | Educator HTTP API definition.

module Dscp.Educator.Web.Educator.API
    ( EducatorApiEndpoints (..)
    , RawEducatorAPI
    , ProtectedEducatorAPI
    , rawEducatorAPI
    , protectedEducatorAPI
    , EducatorApiHandlers
    ) where

import Network.HTTP.Media.MediaType ((//))
import Servant
import Servant.Generic
import Servant.Util (PaginationParams, PaginationSettings (DefPageSize), SortingParamsOf)

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Educator.Auth
import Dscp.Educator.Web.Educator.Error
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Types

data EducatorApiEndpoints route = EducatorApiEndpoints
    { eGetStatus               :: route :- GetStatus
    , eGetStudents             :: route :- GetStudents
    , eAddStudent              :: route :- AddStudent
    , eDeleteStudent           :: route :- DeleteStudent
    , eAddStudentCourse        :: route :- AddStudentCourse
    , eAddStudentAssignment    :: route :- AddStudentAssignment
    , eDeleteStudentAssignment :: route :- DeleteStudentAssignment
    , eGetCourses              :: route :- GetCourses
    , eAddCourse               :: route :- AddCourse
    , eGetCourse               :: route :- GetCourse
    , eGetAssignments          :: route :- GetAssignments
    , eAddAssignment           :: route :- AddAssignment
    , eGetSubmissions          :: route :- GetSubmissions
    , eGetSubmission           :: route :- GetSubmission
    , eDeleteSubmission        :: route :- DeleteSubmission
    , eGetGrades               :: route :- GetGrades
    , eAddGrade                :: route :- AddGrade
    , eGetProofs               :: route :- GetProofs
    , eGetCertificates         :: route :- GetCertificates
    , eGetCertificate          :: route :- GetCertificate
    , eAddCertificate          :: route :- AddCertificate
    } deriving (Generic)

type RawEducatorAPI = ToServant (EducatorApiEndpoints AsApi)

type ProtectedEducatorAPI =
    "api" :> "educator" :> "v1" :>
    Auth' [EducatorAuth, NoAuth "educator"] () :> RawEducatorAPI

type EducatorApiHandlers m = EducatorApiEndpoints (AsServerT m)

rawEducatorAPI :: Proxy RawEducatorAPI
rawEducatorAPI = Proxy

protectedEducatorAPI :: Proxy ProtectedEducatorAPI
protectedEducatorAPI = Proxy

---------------------------------------------------------------------------
-- Content-type 'application/pdf'
---------------------------------------------------------------------------

data PDF

instance Accept PDF where
    contentType _ = "application" // "pdf"
instance MimeRender PDF LByteString where
    mimeRender _ = id
instance MimeUnrender PDF LByteString where
    mimeUnrender _ = Right . id

---------------------------------------------------------------------------
-- General
---------------------------------------------------------------------------

type GetStatus
    = "status"
    :> Get '[DSON] EducatorInfo

---------------------------------------------------------------------------
-- Students
---------------------------------------------------------------------------

type GetStudents
    = "students"
    :> QueryParam "course" Course
    :> QueryParam "isEnrolled" IsEnrolled
    :> QueryFlag "onlyCount"
    :> PaginationParams ('DefPageSize 100)
    :> Summary "Get a list of all registered students' addresses"
    :> Get '[DSON] (Counted StudentInfo)

type AddStudent
    = "students"
    :> Summary "Add a new student address to a database"
    :> ReqBody '[DSON] NewStudent
    :> PostCreated '[DSON] ()

type DeleteStudent
    = "students" :> Capture "student" Student
    :> Summary "Remove a student from a database"
    :> Description "Removes a student only if he's not currently \
                    \attending any course. We will not automatically \
                    \perform a cascade deletion, because it will make \
                    \this operation particularly dangerous. If a student \
                    \attends any course, an error will be raised."
    :> Delete '[DSON] ()

type AddStudentCourse
    = "students" :> Capture "student" Student
    :> "courses"
    :> Summary "Enroll a student in a new course"
    :> Description "Given existing student and course, enroll the \
                    \student to the course."
    :> ReqBody '[DSON] NewStudentCourse
    :> PostCreated '[DSON] ()

type AddStudentAssignment
    = "students" :> Capture "student" Student
    :> "assignments"
    :> Summary "Assign an assignment to a student"
    :> Description "Assigns a new assignment to a student in scope of \
                    \given course."
    :> ReqBody '[DSON] NewStudentAssignment
    :> PostCreated '[DSON] ()

type DeleteStudentAssignment
    = "students"    :> Capture "student" Student
    :> "assignments" :> Capture "assignment" (Hash Assignment)
    :> Summary "Unassign an assignment from a student"
    :> Description "If given student has been assigned a given \
                    \assignment, then unassigns it from them, otherwise \
                    \raises error."
    :> Delete '[DSON] ()

---------------------------------------------------------------------------
-- Courses
---------------------------------------------------------------------------

type GetCourses
    = "courses"
    :> QueryParam "student" Student
    :> QueryFlag "onlyCount"
    :> PaginationParams ('DefPageSize 100)
    :> Summary "Get all courses"
    :> Get '[DSON] (Counted CourseEducatorInfo)

type AddCourse
    = "courses"
    :> Summary "Add a new course to a database"
    :> ReqBody '[DSON] NewCourse
    :> PostCreated '[DSON] Course
    -- TODO: return proper JSON-object here

type GetCourse
    = "course" :> Capture "course" Course
    :> Summary "Get info about a course"
    :> Get '[DSON] CourseEducatorInfo

---------------------------------------------------------------------------
-- Assignments
---------------------------------------------------------------------------

type GetAssignments
    = "assignments"
    :> QueryParam "course" Course
    :> QueryParam "student" Student
    :> QueryParam "isFinal" IsFinal
    :> QueryParam "since" Timestamp
    :> QueryFlag "onlyCount"
    :> PaginationParams ('DefPageSize 100)
    :> Summary "Get all assignments"
    :> Get '[DSON] (Counted AssignmentEducatorInfo)

type AddAssignment
    = "assignments"
    :> Summary "Add assignment to a course"
    :> QueryFlag "autoAssign"
    :> ReqBody '[DSON] NewAssignment
    :> PostCreated '[DSON] ()

---------------------------------------------------------------------------
-- Submissions
---------------------------------------------------------------------------

type GetSubmissions
    = "submissions"
    :> QueryParam "course" Course
    :> QueryParam "student" Student
    :> QueryParam "assignment" (Hash Assignment)
    :> QueryParam "isGraded" IsGraded
    :> QueryParam "since" Timestamp
    :> QueryFlag "onlyCount"
    :> PaginationParams ('DefPageSize 100)
    :> Summary "Get all submissions"
    :> Description "Gets a list of all submissions done by all students. \
                  \This method is inaccessible by students."
    :> Get '[DSON] (Counted SubmissionEducatorInfo)

type GetSubmission
    = "submissions" :> Capture "submission" (Hash Submission)
    :> Summary "Get info about a submission"
    :> Description "Gets a submission data by given submission hash."
    :> Get '[DSON] SubmissionEducatorInfo

type DeleteSubmission
    = "submissions" :> Capture "submission" (Hash Submission)
    :> Summary "Delete a submission"
    :> Description "Deletes a submission from a database. Only ungraded \
                    \submissions can be deleted."
    :> Delete '[DSON] ()

---------------------------------------------------------------------------
-- Grades
---------------------------------------------------------------------------

type GetGrades
    = "grades"
    :> QueryParam "course" Course
    :> QueryParam "student" Student
    :> QueryParam "assignment" (Hash Assignment)
    :> QueryParam "isFinal" IsFinal
    :> QueryParam "since" Timestamp
    :> QueryFlag "onlyCount"
    :> Summary "Get all grades"
    :> Description "Gets a list of all grades performed by all students."
    :> Get '[DSON] (Counted GradeInfo)

type AddGrade
    = "grades"
    :> Summary "Post a new grade"
    :> Description "Posts a new grade with a given body."
    :> ReqBody '[DSON] NewGrade
    :> PostCreated '[DSON] ()

---------------------------------------------------------------------------
-- Proofs
---------------------------------------------------------------------------

type GetProofs
    = "proofs"
    :> QueryParam "course" Course
    :> QueryParam "student" Student
    :> QueryParam "assignment" (Hash Assignment)
    :> QueryFlag "onlyCount"
    :> Summary "Get proofs of all student's activity"
    :> Description "Gets all private transactions related to a student \
                    \together with corresponding Merkle proofs."
    :> Get '[DSON] (Counted BlkProofInfo)

---------------------------------------------------------------------------
-- Certificates
---------------------------------------------------------------------------

type GetCertificates
    = "certificates"
    :> SortingParamsOf Certificate
    :> PaginationParams ('DefPageSize 100)
    :> QueryFlag "onlyCount"
    :> Summary "Get the list of certificates created by Educator"
    :> Description "Gets all the certificates created by Educator. Each \
                   \entry contains certificate metadata and certificate ID."
    :> Get '[DSON] (Counted Certificate)

type GetCertificate
    = "certificate" :> Capture "certificate" (Hash CertificateMeta)
    :> Summary "Get the certificate by ID"
    :> Description "Gets the PDF certificate with FairCV JSON included as \
                   \metadata by ID"
    :> Get '[PDF] LByteString

type AddCertificate
    = "certificates"
    :> Summary "Create a new certificate"
    :> Description "Creates a new certificate given metadata and the grades."
    :> ReqBody '[DSON] CertificateFullInfo
    :> PostCreated '[DSON] Certificate

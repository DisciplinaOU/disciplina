-- | Educator HTTP API definition.

module Dscp.Educator.Web.Educator.API
    ( EducatorApiEndpoints (..)
    , RawEducatorAPI
    , ProtectedEducatorAPI
    , FullEducatorAPI
    , rawEducatorAPI
    , protectedEducatorAPI
    , fullEducatorAPI
    , EducatorApiHandlers
    -- * Re-export for using in packages dependent on `disciplina-educator`
    , PDFBody (..)
    , PDF
    ) where

import Network.HTTP.Media.MediaType ((//))
import Pdf.Scanner (PDFBody (..))
import Servant
-- import Servant.API (Accept (..), MimeRender (..), MimeUnrender (..))
import Servant.API.Generic
import Servant.Server.Generic (AsServerT)
import Servant.Util (ExceptionalResponses, PaginationPageSize (..), PaginationParams,
                     SortingParamsOf, Tag, type (#:))
import Universum

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Educator.Auth
import Dscp.Educator.Web.Educator.Error
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Types
import Dscp.Web.Swagger

--------------------------------------------------------------------------
-- PDF media type
--------------------------------------------------------------------------

data PDF

instance Accept PDF where
    contentType _ = "application" // "pdf"
instance MimeRender PDF PDFBody where
    mimeRender _ = getPDFBody
instance MimeUnrender PDF PDFBody where
    mimeUnrender _ = Right . PDFBody

--------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------

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

type RawEducatorAPI = ToServantApi EducatorApiEndpoints

type ProtectedEducatorAPI =
    Auth' [EducatorAuth, NoAuth "educator"] () :> RawEducatorAPI

type FullEducatorAPI =
    "api" :> "educator" :> "v1" :>
    WithSwaggerUI ProtectedEducatorAPI

type EducatorApiHandlers m = EducatorApiEndpoints (AsServerT m)

rawEducatorAPI :: Proxy RawEducatorAPI
rawEducatorAPI = Proxy

protectedEducatorAPI :: Proxy ProtectedEducatorAPI
protectedEducatorAPI = Proxy

fullEducatorAPI :: Proxy FullEducatorAPI
fullEducatorAPI = Proxy

---------------------------------------------------------------------------
-- General
---------------------------------------------------------------------------

type GetStatus
    = "status"
    :> Tag "General"
    :> Get '[DSON] EducatorInfo

---------------------------------------------------------------------------
-- Students
---------------------------------------------------------------------------

type GetStudents
    = "students"
    :> FilterParam "course" Course
    :> FilterParam "isEnrolled" IsEnrolled
    :> QueryFlag "onlyCount"
    :> PaginationParams ('DefPageSize 20)
    :> Tag "Students"
    :> Summary "Get a list of all registered students' addresses"
    :> Get '[DSON] (Counted StudentInfo)

type AddStudent
    = "students"
    :> Tag "Students"
    :> Summary "Add a new student address to a database"
    :> ReqBody '[DSON] NewStudent
    :> ExceptionalResponses EducatorAPIError
       '[ 409 #: "Student already exists."
        ]
    :> PostCreated '[DSON] ()

type DeleteStudent
    = "students" :> Capture "student" Student
    :> Tag "Students"
    :> Summary "Remove a student from a database"
    :> Description "Removes a student only if he's not currently \
                    \attending any course. We will not automatically \
                    \perform a cascade deletion, because it will make \
                    \this operation particularly dangerous. If a student \
                    \attends any course, an error will be raised."
    :> ExceptionalResponses EducatorAPIError
       '[ 403 #: "Student cannot be deleted, as he is currently attending a course."
        , 404 #: "Student does not exist."
        ]
    :> Delete '[DSON] ()

type AddStudentCourse
    = "students" :> Capture "student" Student
    :> "courses"
    :> Tag "Students"
    :> Tag "Courses"
    :> Summary "Enroll a student in a new course"
    :> Description "Given existing student and course, enroll the \
                    \student to the course."
    :> ReqBody '[DSON] NewStudentCourse
    :> ExceptionalResponses EducatorAPIError
       '[ 403 #: "Course with given ID does not exist."
        , 404 #: "Student with given address not found."
        , 409 #: "Student is already enrolled on this course."
        ]
    :> PostCreated '[DSON] ()

type AddStudentAssignment
    = "students" :> Capture "student" Student
    :> "assignments"
    :> Tag "Students"
    :> Summary "Assign an assignment to a student"
    :> Description "Assigns a new assignment to a student in scope of \
                    \given course."
    :> ReqBody '[DSON] NewStudentAssignment
    :> ExceptionalResponses EducatorAPIError
       '[ 403 #: "Assignment with given hash does not exist."
        , 404 #: "Student with given address not found."
        , 409 #: "Student is already subscribed on the assignment."
        ]
    :> PostCreated '[DSON] ()

type DeleteStudentAssignment
    = "students"    :> Capture "student" Student
    :> "assignments" :> Capture "assignment" (Hash Assignment)
    :> Tag "Students"
    :> Tag "Assignments"
    :> Summary "Unassign an assignment from a student"
    :> Description "If given student has been assigned a given \
                    \assignment, then unassigns it from them, otherwise \
                    \raises error."
    :> ExceptionalResponses EducatorAPIError
       '[ 404 #: "Given student didn't have given assignment."
        ]
    :> Delete '[DSON] ()

---------------------------------------------------------------------------
-- Courses
---------------------------------------------------------------------------

type GetCourses
    = "courses"
    :> FilterParam "student" Student
    :> QueryFlag "onlyCount"
    :> PaginationParams ('DefPageSize 20)
    :> Tag "Courses"
    :> Summary "Get all courses"
    :> Get '[DSON] (Counted CourseEducatorInfo)

type AddCourse
    = "courses"
    :> Tag "Courses"
    :> Summary "Add a new course to a database"
    :> ReqBody '[DSON] NewCourse
    :> PostCreated '[DSON] Course
    -- TODO: return proper JSON-object here

type GetCourse
    = "course" :> Capture "course" Course
    :> Tag "Courses"
    :> Summary "Get info about a course"
    :> ExceptionalResponses EducatorAPIError
       '[ 404 #: "Course with given ID not found."
        ]
    :> Get '[DSON] CourseEducatorInfo

---------------------------------------------------------------------------
-- Assignments
---------------------------------------------------------------------------

type GetAssignments
    = "assignments"
    :> FilterParam "course" Course
    :> FilterParam "student" Student
    :> FilterParam "isFinal" IsFinal
    :> FilterParamSince "since" Timestamp
    :> QueryFlag "onlyCount"
    :> PaginationParams ('DefPageSize 20)
    :> Tag "Assignments"
    :> Summary "Get all assignments"
    :> Get '[DSON] (Counted AssignmentEducatorInfo)

type AddAssignment
    = "assignments"
    :> Summary "Add assignment to a course"
    :> Tag "Assignments"
    :> QueryFlag "autoAssign"
    :> ReqBody '[DSON] NewAssignment
    :> ExceptionalResponses EducatorAPIError
       '[ 409 #: "Exactly the same assignment already exists,"
        ]
    :> PostCreated '[DSON] ()

---------------------------------------------------------------------------
-- Submissions
---------------------------------------------------------------------------

type GetSubmissions
    = "submissions"
    :> FilterParam "course" Course
    :> FilterParam "student" Student
    :> FilterParam "assignment" (Hash Assignment)
    :> FilterParam "isGraded" IsGraded
    :> FilterParamSince "since" Timestamp
    :> QueryFlag "onlyCount"
    :> PaginationParams ('DefPageSize 20)
    :> Tag "Submissions"
    :> Summary "Get all submissions"
    :> Description "Gets a list of all submissions done by all students. \
                  \This method is inaccessible by students."
    :> Get '[DSON] (Counted SubmissionEducatorInfo)

type GetSubmission
    = "submissions" :> Capture "submission" (Hash Submission)
    :> Tag "Submissions"
    :> Summary "Get info about a submission"
    :> Description "Gets a submission data by given submission hash."
    :> ExceptionalResponses EducatorAPIError
       '[ 404 #: "Submission with given hash not found."
        ]
    :> Get '[DSON] SubmissionEducatorInfo

type DeleteSubmission
    = "submissions" :> Capture "submission" (Hash Submission)
    :> Tag "Submissions"
    :> Summary "Delete a submission"
    :> Description "Deletes a submission from a database. Only ungraded \
                    \submissions can be deleted."
    :> ExceptionalResponses EducatorAPIError
       '[ 403 #: "Submission has already been graded and cannot be deleted."
        , 404 #: "Submission with given hash not found."
        ]
    :> Delete '[DSON] ()

---------------------------------------------------------------------------
-- Grades
---------------------------------------------------------------------------

type GetGrades
    = "grades"
    :> FilterParam "course" Course
    :> FilterParam "student" Student
    :> FilterParam "assignment" (Hash Assignment)
    :> FilterParam "isFinal" IsFinal
    :> FilterParamSince "since" Timestamp
    :> QueryFlag "onlyCount"
    :> Tag "Grades"
    :> Summary "Get all grades"
    :> Description "Gets a list of all grades performed by all students."
    :> Get '[DSON] (Counted GradeInfo)

type AddGrade
    = "grades"
    :> Tag "Grades"
    :> Summary "Post a new grade"
    :> Description "Posts a new grade with a given body."
    :> ReqBody '[DSON] NewGrade
    :> PostCreated '[DSON] ()

---------------------------------------------------------------------------
-- Proofs
---------------------------------------------------------------------------

type GetProofs
    = "proofs"
    :> FilterParam "course" Course
    :> FilterParam "student" Student
    :> FilterParam "assignment" (Hash Assignment)
    :> QueryFlag "onlyCount"
    :> Tag "Proofs"
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
    :> PaginationParams ('DefPageSize 20)
    :> QueryFlag "onlyCount"
    :> Tag "Certificates"
    :> Summary "Get the list of certificates created by Educator"
    :> Description "Gets all the certificates created by Educator. Each \
                   \entry contains certificate metadata and certificate ID."
    :> Get '[DSON] (Counted Certificate)

type GetCertificate
    = "certificate" :> Capture "certificate" (Hash CertificateMeta)
    :> Tag "Certificates"
    :> Summary "Get the certificate by ID"
    :> Description "Gets the PDF certificate with FairCV JSON included as \
                   \metadata by ID"
    :> ExceptionalResponses EducatorAPIError
       '[ 404 #: "Certificate with given hash not found."
        ]
    :> Get '[PDF] PDFBody

type AddCertificate
    = "certificates"
    :> Tag "Certificates"
    :> Summary "Create a new certificate"
    :> Description "Creates a new certificate given metadata and the grades."
    :> ReqBody '[DSON] CertificateFullInfo
    :> ExceptionalResponses EducatorAPIError
       '[ 409 #: "Exactly the same certificate already exists."
        ]
    :> PostCreated '[DSON] Certificate

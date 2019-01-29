{-# LANGUAGE TypeInType #-}

-- | Student HTTP API definition.
module Dscp.Educator.Web.Student.API
       ( StudentApiEndpoints (..)
       , ProtectedStudentAPI
       , protectedStudentAPI
       , RawStudentAPI
       , rawStudentAPI
       , StudentApiHandlers
       ) where

import Servant
import Servant.Generic
import Servant.Util (type ( #: ), ExceptionalResponses, PaginationParams, SortingParamsOf, Tag)

import qualified Dscp.Core as Core
import Dscp.Crypto (Hash)
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Student.Auth
import Dscp.Educator.Web.Student.Error (DSON, StudentAPIError)
import Dscp.Educator.Web.Student.Types
import Dscp.Educator.Web.Types

data StudentApiEndpoints route = StudentApiEndpoints
    { sGetCourses       :: route :- GetCourses
    , sGetCourse        :: route :- GetCourse
    , sGetAssignments   :: route :- GetAssignments
    , sGetAssignment    :: route :- GetAssignment
    , sGetSubmissions   :: route :- GetSubmissions
    , sAddSubmission    :: route :- AddSubmission
    , sGetSubmission    :: route :- GetSubmission
    , sDeleteSubmission :: route :- DeleteSubmission
    , sGetProofs        :: route :- GetProofs
    } deriving (Generic)

type RawStudentAPI = ToServant (StudentApiEndpoints AsApi)

type ProtectedStudentAPI =
    "api" :> "student" :> "v1" :>
    Auth' [StudentAuth, NoAuth "student"] Core.Student :> RawStudentAPI

type StudentApiHandlers m = StudentApiEndpoints (AsServerT m)

rawStudentAPI :: Proxy RawStudentAPI
rawStudentAPI = Proxy

protectedStudentAPI :: Proxy ProtectedStudentAPI
protectedStudentAPI = Proxy

---------------------------------------------------------------------------
-- Courses
---------------------------------------------------------------------------

type GetCourses
    = "courses"
    :> QueryParam "isEnrolled" IsEnrolled
    :> QueryFlag "onlyCount"
    :> SortingParamsOf CourseStudentInfo
    :> PaginationParams
    :> Tag "Courses"
    :> Summary "Get Educator's courses"
    :> Description "Gets a list of Educator's courses, both enrolled and available."
    :> Verb 'GET 200 '[DSON] [CourseStudentInfo]

type GetCourse
    = "courses" :> Capture "course" Core.Course
    :> Tag "Courses"
    :> Summary "Get info about the course"
    :> Description "Gets all info about the given course."
    :> ExceptionalResponses StudentAPIError
       '[ 404 #: "Course with given ID not found"
        ]
    :> Verb 'GET 200 '[DSON] CourseStudentInfo

---------------------------------------------------------------------------
-- Assignments
---------------------------------------------------------------------------

type GetAssignments
    = "assignments"
    :> Tag "Assignments"
    :> QueryParam "course" Core.Course
    :> QueryParam "type" Core.DocumentType
    :> QueryParam "isFinal" IsFinal
    :> QueryFlag "onlyCount"
    :> SortingParamsOf AssignmentStudentInfo
    :> PaginationParams
    :> Summary "Get student's assignments"
    :> Description "Gets a list of student's assignments. Filter parameters are \
                   \used to specify specific course, type, etc."
    :> ExceptionalResponses StudentAPIError
       '[ 404 #: "Submission with given hash was not found (or a user has no rights to \
                 \look it up)"
        ]
    :> Verb 'GET 200 '[DSON] [AssignmentStudentInfo]

type GetAssignment
    = "assignments" :> Capture "assignment" (Hash Core.Assignment)
    :> Tag "Assignments"
    :> Summary "Get info about an assignment"
    :> Description "Gets an assignment info by given submission hash. Returns \
                   \404 if a student tries to get an assignment which is not assigned to them."
    :> Verb 'GET 200 '[DSON] AssignmentStudentInfo

---------------------------------------------------------------------------
-- Submissions
---------------------------------------------------------------------------

type GetSubmissions
    = "submissions"
    :> QueryParam "course" Core.Course
    :> QueryParam "assignment" (Hash Core.Assignment)
    :> QueryParam "type" Core.DocumentType
    :> QueryFlag "onlyCount"
    :> SortingParamsOf SubmissionStudentInfo
    :> PaginationParams
    :> Tag "Submissions"
    :> Summary "Get student's submissions"
    :> Description "Gets a list of student's submissions. Filter parameters are \
                   \used to specify specific course, assignment, etc."
    :> Verb 'GET 200 '[DSON] [SubmissionStudentInfo]

type AddSubmission
    = "submissions"
    :> Tag "Submissions"
    :> Summary "Make a new submission"
    :> Description "Posts a new submission with a given body. Request body should \
                   \contain valid student's signature of submission contents, \
                   \otherwise an error will be raised."
    :> ReqBody '[DSON] NewSubmission
    :> ExceptionalResponses StudentAPIError
       '[ 403 #: "Either submission body or submission signature is invalid"
        ]
    :> Verb 'POST 201 '[DSON] SubmissionStudentInfo

type GetSubmission
    = "submissions" :> Capture "submission" (Hash Core.Submission)
    :> Tag "Submissions"
    :> Summary "Get info about a submission"
    :> Description "Gets a submission data by given submission hash. Returns a 404 \
                   \if a student tries to get a submission which is not their own."
   :> ExceptionalResponses StudentAPIError
       '[ 404 #: "Submission with given hash was not found (or a user has no rights to \
                 \look it up)"
        ]
    :> Verb 'GET 200 '[DSON] SubmissionStudentInfo

type DeleteSubmission
    = "submissions" :> Capture "submission" (Hash Core.Submission)
    :> Tag "Submissions"
    :> Summary "Delete a submission"
    :> Description "Deletes a submission from a database. Only ungraded submissions can be deleted."
    :> ExceptionalResponses StudentAPIError
       '[ 403 #: "Cannot delete a graded submission"
        , 404 #: "Submission with given hash not found"
        ]
    :> Verb 'DELETE 200 '[DSON] ()

---------------------------------------------------------------------------
-- Proofs
---------------------------------------------------------------------------

type GetProofs
    = "proofs"
    :> Tag "Proofs"
    :> QueryParam "since" Core.Timestamp
    :> QueryFlag "onlyCount"
    :> Summary "Get available proofs for student"
    :> Description "Gets private transactions together with corresponding Merkle \
                   \subtrees. Transactions from same blocks are grouped together \
                   \and each group has one proof, which is a corresponding Merkle subtree."
    :> Verb 'GET 200 '[DSON] [BlkProofInfo]

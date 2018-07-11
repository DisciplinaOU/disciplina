{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Student HTTP API definition.

module Dscp.Educator.Web.Student.API
       ( StudentAPI
       , studentAPI
       ) where

import Data.Time.Clock (UTCTime)
import Servant

import qualified Dscp.Core.Types as Core
import Dscp.Crypto (Hash)
import Dscp.Educator.Web.Student.Types (Assignment, BlkProof, Course, Submission)

-- TODO: migrate to newer Servant which has `Summary` and `Description` combinators
type StudentAPI
    =    GetCourses
    :<|> GetCourse
    :<|> GetAssignments
    :<|> GetAssignment
    :<|> GetSubmissions
    :<|> GetSubmission
    :<|> MakeSubmission
    :<|> DeleteSubmission
    :<|> GetProofs

studentAPI :: Proxy StudentAPI
studentAPI = Proxy

---------------------------------------------------------------------------
-- Courses
---------------------------------------------------------------------------

type GetCourses
    = "courses"
    :> Summary "Get Educator's courses"
    :> Description "Gets a list of Educator's courses, both enrolled and available."
    :> QueryParam "enrolled" Bool
    :> Verb 'GET 200 '[JSON] [Course]

type GetCourse
    = "courses" :> Capture "courseId" Core.Course
    :> Summary "Get info about the course"
    :> Description "Gets all info about the given course."
    :> Verb 'GET 200 '[JSON] Course

---------------------------------------------------------------------------
-- Assignments
---------------------------------------------------------------------------

type GetAssignments
    = "assignments"
    :> Summary "Get student's assignments"
    :> Description "Gets a list of student's assignments. Filter parameters are \
                   \used to specify specific course, type, etc."
    :> QueryParam "course" Core.Course
    :> QueryParam "type" Core.DocumentType
    :> QueryParam "final" Bool
    :> Verb 'GET 200 '[JSON] [Assignment]

type GetAssignment
    = "assignments" :> Capture "assignmentHash" (Hash Core.Assignment)
    :> Summary "Get info about an assignment"
    :> Description "Gets an assignment info by given submission hash. Returns \
                   \404 if a student tries to get an assignment which is not assigned to them."
    :> Verb 'GET 200 '[JSON] Assignment

---------------------------------------------------------------------------
-- Submissions
---------------------------------------------------------------------------

type GetSubmissions
    = "submissions"
    :> Summary "Get student's submissions"
    :> Description "Gets a list of student's submissions. Filter parameters are \
                   \used to specify specific course, assignment, etc."
    :> QueryParam "course" Core.Course
    :> QueryParam "assignment" (Hash Core.Assignment)
    :> QueryParam "type" Core.DocumentType
    :> Verb 'GET 200 '[JSON] [Submission]

type GetSubmission
    = "submissions" :> Capture "submissionHash" (Hash Core.Submission)
    :> Summary "Get info about a submission"
    :> Description "Gets a submission data by given submission hash. Returns a 404 \
                   \if a student tries to get a submission which is not their own."
    :> Verb 'GET 200 '[JSON] Submission

type MakeSubmission
    = "submissions"
    :> Summary "Make a new submission"
    :> Description "Posts a new submission with a given body. Request body should \
                   \contain valid student's signature of submission contents, \
                   \otherwise an error will be raised."
    :> ReqBody '[JSON] Core.SignedSubmission
    :> Verb 'POST 200 '[JSON] Submission

type DeleteSubmission
    = "submissions" :> Capture "submissionHash" (Hash Core.Submission)
    :> Summary "Delete a submission"
    :> Description "Deletes a submission from a database. Only ungraded submissions can be deleted."
    :> Verb 'DELETE 200 '[JSON] ()

type GetProofs
    = "proofs"
    :> Summary "Get available proofs for student"
    :> Description "Gets private transactions together with corresponding Merkle \
                   \subtrees. Transactions from same blocks are grouped together \
                   \and each group has one proof, which is a corresponding Merkle subtree."
    :> QueryParam "since" UTCTime
    :> Verb 'GET 200 '[JSON] [BlkProof]

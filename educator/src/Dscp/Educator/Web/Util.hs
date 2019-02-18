{-# LANGUAGE ExistentialQuantification #-}

-- | Utils for educator servers.

module Dscp.Educator.Web.Util
     ( domainErrorToShortJSON
     , domainToServantErrNoReason
     ) where

import Servant (ServantErr, err400, err403, err404, err409, err500)

import Dscp.Educator.DB (DatabaseSemanticError (..), DomainError (..), DomainErrorItem (..))

-- | Mention only constructor name on JSON, used in errors representation.
domainErrorToShortJSON :: DomainError -> Text
domainErrorToShortJSON = \case
    AbsentError dom -> case dom of
        CourseDomain{}                        -> "CourseNotFound"
        StudentDomain{}                       -> "StudentNotFound"
        AssignmentDomain{}                    -> "AssignmentNotFound"
        StudentCourseEnrollmentDomain{}       -> "StudentDoesNotAttendCourse"
        StudentAssignmentSubscriptionDomain{} -> "StudentHasNoAssignment"
        SubmissionDomain{}                    -> "SubmissionNotFound"
        TransactionDomain{}                   -> "TransactionNotFound"
        BlockWithIndexDomain{}                -> "BlockWithIndexNotFound"
        CertificateDomain{}                   -> "CertificateNotFound"
    AlreadyPresentError dom -> case dom of
        CourseDomain{}                        -> "CourseAlreadyExists"
        StudentDomain{}                       -> "StudentAlreadyExists"
        AssignmentDomain{}                    -> "AssignmentAlreadyExists"
        StudentCourseEnrollmentDomain{}       -> "StudentAlreadyAttendsCourse"
        StudentAssignmentSubscriptionDomain{} -> "StudentAlreadyHasAssignment"
        SubmissionDomain{}                    -> "SubmissionAlreadyExists"
        TransactionDomain{}                   -> "TransactionAlreadyExists"
        BlockWithIndexDomain{}                -> "BlockWithIndexAlreadyExists"
        CertificateDomain{}                   -> "CertificateAlreadyExists"
    SemanticError err -> case err of
        StudentIsActiveError{}     -> "StudentIsActive"
        DeletingGradedSubmission{} -> "DeletingGradedSubmission"

domainToServantErrNoReason :: DomainError -> ServantErr
domainToServantErrNoReason = \case
    AbsentError dom -> case dom of
        CourseDomain{}                        -> err404
        StudentDomain{}                       -> err404
        AssignmentDomain{}                    -> err404
        StudentCourseEnrollmentDomain{}       -> err400
        StudentAssignmentSubscriptionDomain{} -> err400
        SubmissionDomain{}                    -> err404
        TransactionDomain{}                   -> err404
        BlockWithIndexDomain{}                -> err500
        CertificateDomain{}                   -> err404
    AlreadyPresentError _ -> err409
    SemanticError{} -> err403

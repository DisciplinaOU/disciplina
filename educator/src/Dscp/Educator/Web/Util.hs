{-# LANGUAGE ExistentialQuantification #-}

-- | Utils for educator servers.

module Dscp.Educator.Web.Util () where

import Servant (err400, err403, err404, err409, err500)

import Dscp.Educator.DB (DatabaseSemanticError (..), DomainError (..), DomainErrorItem (..))
import Dscp.Util
import Dscp.Util.Constructors
import Dscp.Web.Class (HasErrorTag (..), ToServantErr (..))
import Dscp.Web.Swagger

instance HasErrorTag DomainError where
    errorTag = \case
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
        SemanticError err -> errorTag err

instance HasErrorTag DatabaseSemanticError where
    errorTag = \case
        StudentIsActiveError{}     -> "StudentIsActive"
        DeletingGradedSubmission{} -> "DeletingGradedSubmission"

instance ToServantErr DomainError where
    toServantErrNoBody = \case
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
        SemanticError err -> toServantErrNoBody err

instance ToServantErr DatabaseSemanticError where
    toServantErrNoBody _ = err403

instance EnumHasDescription DomainError where
    enumDocDescription = gEnumDocDesc $ \case
        AbsentError err ->
            mapMaybe (errorDocNoDesc . AbsentError) $
            enlistConstructorsOf @UnsafeFiller err

        AlreadyPresentError err ->
            mapMaybe (errorDocNoDesc . AlreadyPresentError) $
            enlistConstructorsOf @UnsafeFiller err

        SemanticError err -> errorCaseDocDesc @UnsafeFiller (proxyOf err) $ \case
            StudentIsActiveError{} ->
                "Student cannot be deleted because he is attending a course."
            DeletingGradedSubmission{} ->
                "Attempt to delete an already graded submission."

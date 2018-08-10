{-# LANGUAGE ExistentialQuantification #-}

-- | Utils for educator servers.

module Dscp.Educator.Web.Util
     ( -- * Errors
       domainErrorToShortJSON
     , domainToServantErrNoReason

       -- * SQLite helpers
     , FilterClause
     , SomeParams (..)
     , oneParam
     , mkFilterOn
     , filterClauses
     , mkDocTypeFilter

     , FetchIf
     , positiveFetch
     ) where

import Data.Singletons.Bool (SBoolI, fromSBool, sbool)
import Database.SQLite.Simple ((:.) (..), FromRow (..), Only (..), Query, ToRow (..))
import Database.SQLite.Simple.ToField (ToField)
import Servant (ServantErr, err400, err403, err404, err409, err500)

import Dscp.Core
import Dscp.DB.SQLite (DatabaseSemanticError (..), DomainError (..), DomainErrorItem (..))

---------------------------------------------------------------------
-- Errors
---------------------------------------------------------------------

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
    AlreadyPresentError dom -> case dom of
        CourseDomain{}                        -> "CourseAlreadyExists"
        StudentDomain{}                       -> "StudentAlreadyExists"
        AssignmentDomain{}                    -> "AssignmentAlreadyExists"
        StudentCourseEnrollmentDomain{}       -> "StudentAlreadyAttendsCourse"
        StudentAssignmentSubscriptionDomain{} -> "StudentAlreadyHasAssignment"
        SubmissionDomain{}                    -> "SubmissionAlreadyExists"
        TransactionDomain{}                   -> "TransactionAlreadyExists"
        BlockWithIndexDomain{}                -> "BlockWithIndexAlreadyExists"
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
    AlreadyPresentError _ -> err409
    SemanticError{} -> err403

---------------------------------------------------------------------
-- SQLite helpers: filtering
---------------------------------------------------------------------

-- | One of conditions within conjenction sum after @where@.
newtype FilterClause = FilterClause { unFilterClause :: String }
    deriving (IsString)

-- | Contains a list of objects forming a row in SQL table.
data SomeParams = forall a. ToRow a => SomeParams a

instance Semigroup SomeParams where
    SomeParams a <> SomeParams b = SomeParams (a :. b)

instance Monoid SomeParams where
    mempty = SomeParams ()
    mappend = (<>)

instance ToRow SomeParams where
    toRow (SomeParams a) = toRow a

-- | Lift a single field to 'SomeParams'.
oneParam :: ToField a => a -> SomeParams
oneParam = SomeParams . Only

-- | For SQL table field and optional value, makes a clause to select
-- only rows with field mathing value (if present).
-- This function helps to deal with sqlite feature of passing parameters
-- seperately from query itself.
mkFilterOn :: ToField a => String -> Maybe a -> (FilterClause, SomeParams)
mkFilterOn fieldName mparam = case mparam of
      Just param -> (FilterClause ("and " <> fieldName <> "= ?"),
                     oneParam param)
      Nothing    -> ("", mempty)

-- | Attaches filtering @where@ clauses to query.
-- "where" statement with some condition should go last in the query.
filterClauses :: Query -> [FilterClause] -> Query
filterClauses queryText fs =
    mconcat $ queryText : map (fromString . unFilterClause) fs
infixl 1 `filterClauses`

-- | Create filter for 'DocumentType'.
mkDocTypeFilter :: String -> Maybe DocumentType -> (FilterClause, SomeParams)
mkDocTypeFilter fieldName = \case
    Nothing ->
        ("", mempty)
    Just Offline ->
        (FilterClause $ "and " <> fieldName <> " = ?", oneParam offlineHash)
    Just Online  ->
        (FilterClause $ "and " <> fieldName <> " <> ?", oneParam offlineHash)

---------------------------------------------------------------------
-- SQLite helpers: fields selection
---------------------------------------------------------------------

-- | Pack of fields which are only parsed if 'required' is set to 'True'.
newtype FetchIf (required :: Bool) a = FetchIf (Maybe a)

-- | Get the value if it was required.
positiveFetch :: (required ~ 'True) => FetchIf required a -> a
positiveFetch (FetchIf a) = a ?: error "positiveFetch: Nothing"

instance (FromRow a, SBoolI required) => FromRow (FetchIf required a) where
    fromRow
        | fromSBool (sbool @required) = FetchIf . Just <$> fromRow
        | otherwise                   = pure (FetchIf Nothing)

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Common queries for student and educator APIs.

module Dscp.Educator.Web.Queries
    ( eqDocTypeQ
    , isPositiveGradeQ
    , commonExistsSubmission
    , isGradedSubmission
    , commonDeleteSubmission
    ) where

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQL
import Dscp.Educator.DB
import Dscp.Educator.Web.Types

es :: DatabaseSettings be EducatorSchema
es = educatorSchema

----------------------------------------------------------------------------
-- Filters
----------------------------------------------------------------------------

eqDocTypeQ
    :: _
    => DocumentType a
    -> QGenExpr syntax ctx s (Hash Raw)
    -> QGenExpr syntax ctx s Bool
eqDocTypeQ docType contentsHash = case docType of
    Offline -> contentsHash ==. val_ offlineHash
    Online  -> contentsHash /=. val_ offlineHash

-- | Check whether a corresponding assignment is successfully passed.
isPositiveGradeQ
    :: _
    => QGenExpr context syntax s Grade -> QGenExpr context syntax s Bool
isPositiveGradeQ _ = val_ True

----------------------------------------------------------------------------
-- Predicates
----------------------------------------------------------------------------

-- | Whether a submission exists.
-- If student is supplied, then submissions owned by other students won't be visible.
commonExistsSubmission
    :: MonadEducatorWebQuery m
    => Hash Submission
    -> Maybe Student
    -> DBT t m Bool
commonExistsSubmission submissionH studentF =
    checkExists $ do
        submission <- all_ (esSubmissions es)
        guard_ (valPk_ submissionH `references_` submission)
        guard_ $ filterMatchesPk_ studentF (srStudent submission)

-- | Whether a submission has taken any grade.
isGradedSubmission
    :: MonadEducatorWebQuery m
    => Hash Submission -> DBT t m Bool
isGradedSubmission submissionH =
    checkExists $ do
        privateGrade <- all_ (esGrades es)
        guard_ (grSubmission privateGrade ==. valPk_ submissionH)

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

-- | Delete a submission. If absent, error is thrown.
-- If student is supplied, then submissions owned by other students won't be
-- visible/affected.
commonDeleteSubmission
    :: MonadEducatorWebQuery m
    => Hash Submission
    -> Maybe Student
    -> DBT t m ()
commonDeleteSubmission submissionH studentF =
    rewrapReferenceGotInvalid (SemanticError $ DeletingGradedSubmission submissionH) $ do
        changes <- runDelete $ delete (esSubmissions es)
            (\submission -> valPk_ submissionH `references_` submission
                        &&. filterMatchesPk_ studentF (srStudent submission))

        unless (anyAffected changes) $
            throwM $ AbsentError (SubmissionDomain submissionH)

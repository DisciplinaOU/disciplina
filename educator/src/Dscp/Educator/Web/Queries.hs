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
import Dscp.DB.SQLite
import Dscp.Educator.DB
import Dscp.Educator.Web.Types
import Dscp.Util

es :: DatabaseSettings be EducatorSchema
es = educatorSchema

----------------------------------------------------------------------------
-- Filters
----------------------------------------------------------------------------

eqDocTypeQ
    :: _
    => DocumentType
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
    -> m Bool
commonExistsSubmission submissionH studentF =
    checkExists $ do
        submission <- all_ (esSubmissions es)
        guard_ (valPk_ submissionH `references_` submission)
        guard_ $ filterMatchesPk_ studentF (srStudent submission)

-- | Whether a submission has taken any grade.
isGradedSubmission
    :: MonadEducatorWebQuery m
    => Hash Submission -> m Bool
isGradedSubmission submissionH =
    checkExists $ do
        privateTx <- all_ (esTransactions es)
        guard_ (trSubmission privateTx ==. valPk_ submissionH)

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

-- | Delete a submission. If absent, error is thrown.
-- If student is supplied, then submissions owned by other students won't be
-- visible/affected.
commonDeleteSubmission
    :: (MonadEducatorWebQuery m, WithinWriteTx)
    => Hash Submission
    -> Maybe Student
    -> m ()
commonDeleteSubmission submissionH studentF = do
    commonExistsSubmission submissionH studentF
        `assert` AbsentError (SubmissionDomain submissionH)
    rewrapReferenceGotInvalid (SemanticError $ DeletingGradedSubmission submissionH) $
        runDelete $ delete (esSubmissions es)
            (\submission -> valPk_ submissionH `references_` submission
                        &&. filterMatchesPk_ studentF (srStudent submission))

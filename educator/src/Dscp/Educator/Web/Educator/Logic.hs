module Dscp.Educator.Web.Educator.Logic
    ( MonadEducatorWeb

    , educatorGetSubmission
    , educatorGetAllSubmissions
    ) where

import Data.Default (def)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQLite
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Types
import Dscp.Util

educatorGetSubmission
    :: MonadEducatorWebQuery m
    => Hash Submission
    -> DBT r m SubmissionEducatorInfo
educatorGetSubmission submissionH = do
    commonGetSubmissions EducatorCase def{ sfSubmissionHash = Just submissionH }
        >>= listToMaybeWarn "submission"
        >>= nothingToThrow (AbsentError $ SubmissionDomain submissionH)

educatorGetAllSubmissions
    :: MonadEducatorWebQuery m
    => DBT r m [SubmissionEducatorInfo]
educatorGetAllSubmissions = commonGetSubmissions EducatorCase def

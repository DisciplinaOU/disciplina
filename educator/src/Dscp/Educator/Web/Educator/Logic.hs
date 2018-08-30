module Dscp.Educator.Web.Educator.Logic
    ( EducatorApiWorkMode

    , educatorGetSubmission
    , educatorGetAllSubmissions
    ) where

import Data.Default (def)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQLite
import Dscp.Educator.Web.Educator.Queries
import Dscp.Educator.Web.Educator.Types
import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Types
import Dscp.Util

type EducatorApiWorkMode m =
    ( MonadCatch m
    , MonadSQLiteDB m
    , MonadEducatorAPIQuery m
    )

educatorGetSubmission
    :: EducatorApiWorkMode m
    => Hash Submission
    -> m SubmissionEducatorInfo
educatorGetSubmission submissionH = do
    commonGetSubmissions EducatorCase def{ sfSubmissionHash = Just submissionH }
        >>= listToMaybeWarn "submission"
        >>= nothingToThrow (AbsentError $ SubmissionDomain submissionH)

educatorGetAllSubmissions
    :: EducatorApiWorkMode m
    => m [SubmissionEducatorInfo]
educatorGetAllSubmissions = commonGetSubmissions EducatorCase def

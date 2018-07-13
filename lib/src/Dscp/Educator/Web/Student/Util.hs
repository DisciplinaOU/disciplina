-- | Temporal mess of functions which didn't get into any other module.

module Dscp.Educator.Web.Student.Util
    ( verifySignedSubmission
    , notImplemented
    ) where

import Dscp.Core.Types (SignedSubmission)
import Dscp.Educator.Web.Student.Error (APIError (..))
import Dscp.Educator.Web.Student.Types (Student)
import Servant (err500, errBody)
import qualified UnliftIO as UIO

-- | Checks that
-- 1. 'SignedSubmission' is valid;
-- 2. It was actually signed by a student.
verifySignedSubmission :: Student -> SignedSubmission -> Either APIError ()
verifySignedSubmission _ _ = pass  -- TODO [DSCP-141]

notImplemented :: MonadIO m => m a
notImplemented = UIO.throwIO $ err500 { errBody = "Not implemented" }

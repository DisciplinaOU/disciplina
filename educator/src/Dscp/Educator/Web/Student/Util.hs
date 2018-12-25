-- | Temporal mess of functions which didn't get into any other module.

module Dscp.Educator.Web.Student.Util
    ( verifyStudentSubmission
    , notImplemented
    ) where

import Control.Monad.Error.Class (throwError)
import Servant (err500, errBody)
import qualified UnliftIO as UIO

import Dscp.Core
import Dscp.Educator.Web.Student.Error (WrongSubmissionSignature (..))

-- | Checks that
-- 1. 'SignedSubmission' is valid;
-- 2. It was actually signed by a student who makes a request.
verifyStudentSubmission
    :: Student
    -> SignedSubmission
    -> Either WrongSubmissionSignature ()
verifyStudentSubmission author ss = do
    let signatureAuthor = _sStudentId (_ssSubmission ss)
    unless (signatureAuthor == author) $
        throwError FakeSubmissionSignature
    validateSubmission ss
        & first SubmissionSignatureInvalid

notImplemented :: MonadIO m => m a
notImplemented = UIO.throwIO $ err500 { errBody = "Not implemented" }

module Dscp.Web.Class
    ( ErrorTag
    , HasErrorTag (..)
    , ToServantErrNoReason (..)
    ) where

import Servant (ServantErr)

-- | Error kind identifier.
type ErrorTag = Text

-- | For error type appearing in API, allows to fetch corresponding error
-- identifier.
class HasErrorTag e where
    errorTag :: e -> ErrorTag

-- | Get a servant error with appropriate HTTP code, but without
-- reason field filled.
class ToServantErrNoReason e where
    toServantErrNoReason :: e -> ServantErr

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Educator HTTP API definition.

module Dscp.MultiEducator.Web.Educator.API
    ( MultiEducatorAPI
    , multiEducatorAPI
    , MultiStudentAPI
    , multiStudentAPI
    ) where

import Servant

import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Educator.API
import Dscp.Educator.Web.Student.API
import Dscp.MultiEducator.Web.Educator.Auth

type MultiEducatorAPI =
    "api" :> "educator" :> "v1" :> ProtectedMultiEducatorAPI

type ProtectedMultiEducatorAPI =
    Auth' '[MultiEducatorAuth, NoAuth "multi-educator"] EducatorAuthData :> RawEducatorAPI

type MultiStudentAPI = Capture "educator" Text :> ProtectedStudentAPI

multiEducatorAPI :: Proxy MultiEducatorAPI
multiEducatorAPI = Proxy

multiStudentAPI :: Proxy MultiStudentAPI
multiStudentAPI = Proxy

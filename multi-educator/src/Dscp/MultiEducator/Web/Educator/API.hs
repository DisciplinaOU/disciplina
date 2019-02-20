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

type MultiEducatorAPI = ProtectedMultiEducatorAPI

type ProtectedMultiEducatorAPI = Auth' '[MultiEducatorAuth] EducatorAuthToken :> RawEducatorAPI

type MultiStudentAPI = Capture "educator" Text :> ProtectedStudentAPI

multiEducatorAPI :: Proxy MultiEducatorAPI
multiEducatorAPI = Proxy

multiStudentAPI :: Proxy MultiStudentAPI
multiStudentAPI = Proxy

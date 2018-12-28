{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Educator HTTP API definition.

module Dscp.MultiEducator.Web.Educator.API
    ( MultiEducatorApiEndpoints (..)
    , MultiEducatorAPI
    , MultiEducatorAuthAPI
    , multiEducatorAPI
    , MultiStudentAPI
    , multiStudentAPI
    , MultiEducatorApiHandlers
    ) where

import Servant
import Servant.Auth.Server
import Servant.Generic

--import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Educator.API
import Dscp.Educator.Web.Educator.Error
import Dscp.Educator.Web.Student.API
import Dscp.MultiEducator.Web.Educator.Auth
import Dscp.MultiEducator.Web.Educator.Types

type MultiEducatorAuthAPI =
    "api" :> "educator" :> "v1" :> ToServant (MultiEducatorApiEndpoints AsApi)

type MultiEducatorAPI = MultiEducatorAuthAPI :<|> ProtectedMultiEducatorAPI

type ProtectedMultiEducatorAPI = Auth '[JWT] EducatorAuthData :> RawEducatorAPI

type MultiEducatorApiHandlers m = MultiEducatorApiEndpoints (AsServerT m)

type MultiStudentAPI = Capture "educator" Text :> ProtectedStudentAPI

multiEducatorAPI :: Proxy MultiEducatorAPI
multiEducatorAPI = Proxy

multiStudentAPI :: Proxy MultiStudentAPI
multiStudentAPI = Proxy

-- TODO [DSCP-176]: add a way to fetch ALL assignments, even whose which are not assigned to any student

data MultiEducatorApiEndpoints route = MultiEducatorApiEndpoints
    {
      -- Students

      meLogin :: route
        :- "login"
        :> Summary "Login with a name and password"
        :> ReqBody '[DSON] LoginData
        :> PostNoContent '[DSON] (Headers '[ Header "Set-Cookie" SetCookie
                                           , Header "Set-Cookie" SetCookie] NoContent)

    , meRegister :: route
        :- "register"
        :> Summary "Register a new educator with a login and password"
        :> ReqBody '[DSON] LoginData
        :> PostNoContent '[DSON] (Headers '[ Header "Set-Cookie" SetCookie
                                           , Header "Set-Cookie" SetCookie] NoContent)

   } deriving (Generic)

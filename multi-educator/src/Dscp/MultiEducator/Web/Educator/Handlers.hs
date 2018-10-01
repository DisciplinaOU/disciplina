-- | Educator API handlers

module Dscp.MultiEducator.Web.Educator.Handlers
       ( multiEducatorApiHandlers
       ) where

import Servant (err401, NoContent(..))
import Servant.Auth.Server (JWTSettings, CookieSettings, acceptLogin)

import Dscp.MultiEducator.Launcher.Mode
import Dscp.MultiEducator.Web.Educator.API
import Dscp.MultiEducator.Web.Educator.Types
import Dscp.MultiEducator.Web.Educator.Auth

multiEducatorApiHandlers
    :: forall m ctx. MultiEducatorWorkMode ctx m
    => JWTSettings
    -> CookieSettings
    -> MultiEducatorApiHandlers m
multiEducatorApiHandlers jwtSettings cookieSettings =
    MultiEducatorApiEndpoints
    {
      meLogin = createOrLogin False
    , meRegister = createOrLogin True
    }
  where
    createOrLogin create (LoginData ldLogin ldPassword) = do
        result <- loadEducator create ldLogin ldPassword
        mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings (EducatorAuthData ldLogin)
        case (result,) <$> mApplyCookies of
            Just (True, applyCookies) -> return $ applyCookies NoContent
            _ -> throwM err401


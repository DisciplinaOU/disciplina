{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Necessary types and implementation for Student authenthication
module Dscp.Educator.Web.Student.Auth
       ( StudentCheckAction (..)
       , mkStudentActionM
       , StudentAuth
       , ClientAuthData (StudentClientAuthData)
       ) where

import Servant.Auth.Server (AuthCheck (..))
import Servant.Auth.Server.Internal.Class (AuthArgs (..), IsAuth (..))
import UnliftIO (MonadUnliftIO, UnliftIO (..), askUnliftIO)

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Auth

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | Custom authentication type for auth-servant
data StudentAuth

-- | Action to get students' public keys
newtype StudentCheckAction = StudentCheckAction
    { runStudentCheckAction :: Address -> IO Bool
    }

instance IsAuth StudentAuth Student where
    type AuthArgs StudentAuth = '[StudentCheckAction]
    runAuth _ _ = studentAuthCheck

instance IsClientAuth StudentAuth where
    data ClientAuthData StudentAuth = StudentClientAuthData SecretKey
    provideAuth req (StudentClientAuthData sk) = signRequestBasic sk req

-- | Creates a check action supplying it with the current context.
mkStudentActionM :: MonadUnliftIO m => (Address -> m Bool) -> m StudentCheckAction
mkStudentActionM action = do
    UnliftIO unliftIO <- askUnliftIO
    return . StudentCheckAction $ \pk -> unliftIO (action pk)

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

-- | This function returns AuthCheck that checks the signature of the JWT.
studentAuthCheck :: StudentCheckAction -> AuthCheck Student
studentAuthCheck (StudentCheckAction checkStudent) = do
    pk <- checkAuthBasic
    good <- liftIO $ checkStudent (mkAddr pk)
    guard good
    return $ mkAddr pk

---------------------------------------------------------------------------
-- NoAuth
---------------------------------------------------------------------------

type instance NoAuthData "student" = Student

---------------------------------------------------------------------------
-- Documentation
---------------------------------------------------------------------------

instance AuthHasSwagger StudentAuth where
    authSecurityDoc = jwtSecurityDoc educatorAuthDocDesc

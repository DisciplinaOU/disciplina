{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Necessary types and implementation for Student authenthication
module Dscp.Educator.Web.Student.Auth
       ( GetStudentsAction (..)
       , StudentAuth
       ) where

import Servant.Auth.Server (AuthCheck (..))
import Servant.Auth.Server.Internal.Class (AuthArgs (..), IsAuth (..))

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Auth

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | Custom authentication type for auth-servant
data StudentAuth

-- | Action to get students' public keys
newtype GetStudentsAction = GetStudentsAction (IO [PublicKey])

instance IsAuth StudentAuth Student where
    type AuthArgs StudentAuth = '[GetStudentsAction]
    runAuth _ _ = studentAuthCheck

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

-- | This function returns AuthCheck that checks the signature of the JWT.
studentAuthCheck :: GetStudentsAction -> AuthCheck Student
studentAuthCheck (GetStudentsAction getStudents) = do
    pk <- checkAuthBasic
    students <- liftIO getStudents
    asum $ map (\pk' -> guard (pk `constTimeEq` pk')) students
    return $ mkAddr pk

---------------------------------------------------------------------------
-- NoAuth
---------------------------------------------------------------------------

type instance NoAuthData "student" = Student

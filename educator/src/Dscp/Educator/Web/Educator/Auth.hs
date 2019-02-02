{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Necessary types and implementation for Educator authenthication
module Dscp.Educator.Web.Educator.Auth
       ( EducatorPublicKey (..)
       , EducatorAuth
       ) where

import Servant.Auth.Server (AuthCheck (..))
import Servant.Auth.Server.Internal.Class (AuthArgs (..), IsAuth (..))

import Dscp.Crypto
import Dscp.Educator.Web.Auth

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | Custom authentication type for auth-servant
data EducatorAuth

-- | Type that holds Educator's public key
newtype EducatorPublicKey = EducatorPublicKey PublicKey

instance IsAuth EducatorAuth () where
    type AuthArgs EducatorAuth = '[EducatorPublicKey]
    runAuth _ _ = educatorAuthCheck

instance IsClientAuth EducatorAuth where
    data ClientAuthData EducatorAuth = EducatorClientAuthData SecretKey
    provideAuth req (EducatorClientAuthData sk) = signRequestBasic sk req

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

-- | This function returns AuthCheck that checks the signature of the JWT.
educatorAuthCheck :: EducatorPublicKey -> AuthCheck ()
educatorAuthCheck (EducatorPublicKey pk) = do
    otherPk <- checkAuthBasic
    -- Remember about timing attacks
    guard (pk `constTimeEq` otherPk)

---------------------------------------------------------------------------
-- NoAuth
---------------------------------------------------------------------------

type instance NoAuthData "educator" = ()

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Necessary types and implementation for Educator authenthication
module Dscp.Educator.Web.Educator.Auth
       ( EducatorPublicKey (..)
       , EducatorAuth
       ) where

import Crypto.JOSE.JWK (KeyMaterial (..), KeyOp (..), OKPKeyParameters (..), jwkKeyOps)
import Crypto.JWT (fromKeyMaterial)
import Servant.Auth.Server (AuthCheck (..), defaultJWTSettings, jwtAuthCheck)
import Servant.Auth.Server.Internal.Class (AuthArgs (..), IsAuth (..))

import Dscp.Crypto (AbstractPK (..), PublicKey)
import Dscp.Educator.Web.Auth (WithCommonAuthData (..))

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | Custom authentication type for auth-servant
data EducatorAuth

-- | Type that holds Educator's public key
newtype EducatorPublicKey = EducatorPublicKey PublicKey

instance IsAuth EducatorAuth (WithCommonAuthData ()) where
    type AuthArgs EducatorAuth = '[EducatorPublicKey]
    runAuth _ _ = educatorAuthCheck

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

-- | This function returns AuthCheck that checks the signature of the JWT.
educatorAuthCheck :: EducatorPublicKey -> AuthCheck (WithCommonAuthData ())
educatorAuthCheck (EducatorPublicKey (AbstractPK pub)) = do
    let jwk =
          fromKeyMaterial (OKPKeyMaterial $ Ed25519Key pub Nothing) & jwkKeyOps .~ Just [Verify]
    authData <- jwtAuthCheck . defaultJWTSettings $ jwk
    return $ WithCommonAuthData authData ()

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Necessary types and implementation for Educator authenthication
module Dscp.MultiEducator.Web.Educator.Auth
       ( LookupEducator (..)
       , EducatorAuthResult (..)
       , EducatorAuthData (..)
       ) where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Servant.Auth.Server (FromJWT, ToJWT)

import Dscp.DB.SQLite (SQL)
import Dscp.Educator.Launcher.Mode (EducatorNode)
import Dscp.Resource.Keys (KeyResources)

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- data EducatorAuth

newtype LookupEducator = LookupEducator (Text -> IO (Maybe (KeyResources EducatorNode, SQL)))

data EducatorAuthResult
    = EducatorNotFound
    | EducatorData (KeyResources EducatorNode) SQL

newtype EducatorAuthData = EducatorAuthData
    { eadLogin :: Text
    }

deriveJSON defaultOptions ''EducatorAuthData

instance FromJWT EducatorAuthData
instance ToJWT EducatorAuthData

{-
instance IsAuth EducatorAuth EducatorAuthResult where
    type AuthArgs EducatorAuth = '[LookupEducator, JWTSettings]
    runAuth _ _ = multiEducatorAuthCheck

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

-- | This function returns AuthCheck that checks the signature of the JWT.
multiEducatorAuthCheck :: LookupEducator -> JWTSettings -> AuthCheck EducatorAuthResult
multiEducatorAuthCheck (LookupEducator lookupEducator) jwtSettings = do
    (JWTData login) <- jwtAuthCheck jwtSettings
    liftIO (lookupEducator login) >>= \case
        Just (key, db) -> return $ EducatorData key db
        Nothing -> return $ EducatorNotFound

-}

-- | Necessary types and implementation for educator web server authenthication
module Dscp.Educator.Web.Auth
       ( AuthData (..)
       , checkAuthBasic
       , checkAuthData
       , createAuthToken
       , makeTestAuthToken
       , signRequestBasic

       , AuthHasSwagger (..)
       , jwtSecurityDoc
       , educatorAuthDocDesc
       ) where

import Control.Lens (at, (<>~), (?~))
import Data.Aeson (decodeStrict)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Swagger as S
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Typeable (typeRep)
import Servant ((:>))
import Servant.Auth.Server (AuthCheck (..))
import qualified Servant.Client.Core.Internal.Request as Cli
import Servant.Swagger (HasSwagger (..))
import System.FilePath.Posix ((</>))

import Dscp.Crypto
import Dscp.Util.FileEmbed
import Dscp.Util.Servant

---------------------------------------------------------------------------
-- Data types
---------------------------------------------------------------------------

-- | A type that reperesents the data that the client has sent to authenthicate
data AuthData = AuthData
    { adPath :: !Text
    , adTime :: !UTCTime
    }
    deriving Generic

deriveJSON defaultOptions ''AuthData

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

checkAuthBasic :: AuthCheck PublicKey
checkAuthBasic = do
    (pk, payload) <- checkJWitness
    authData <- maybe mempty pure $ decodeStrict payload
    checkAuthData authData
    pure pk

checkAuthData :: Maybe AuthTimeout -> AuthData -> AuthCheck ()
checkAuthData mAuthTimeout AuthData {..} = do
    request <- ask
    -- request path verification
    guard (adPath == requestEndpoint request)
    -- time verification
    checkAuthTime adTime mAuthTimeout

-- | Create an authentication token.
createAuthToken :: MonadIO m => SecretKey -> Text -> m ByteString
createAuthToken secretKey endpoint = do
    time <- liftIO getCurrentTime
    let authData = AuthData
            { adPath = endpoint
            , adTime = time
            }
    return $ authDataToJWT secretKey authData

-- | Make authentication signature.
signRequestBasic :: SecretKey -> Cli.Request -> IO AuthToken
signRequestBasic sk req = do
    let endpoint = decodeUtf8 . toLazyByteString $ Cli.requestPath req
    token <- createAuthToken sk endpoint
    return $ toAuthTokenBearer token


-- | Make a test authentication token.
-- You can pass this to @curl@ as @-H "Authorization: Bearer <produced text>"@.
makeTestAuthToken :: SecretKey -> Text -> ByteString
makeTestAuthToken secretKey endpoint = authDataToJWT secretKey authData
  where
    farFuture = posixSecondsToUTCTime 1735689600 -- 1 Jan 2025
    authData = AuthData { adPath = endpoint, adTime = farFuture }

instance AuthHasSwagger (NoAuth s) where
    authNameDoc = "NoAuth"
    authSecurityDoc = NoAuthSwaggerInfo

---------------------------------------------------------------------------
-- Documentation
---------------------------------------------------------------------------

data AuthSwaggerInfo
    = AuthSwaggerInfo
        { asiRequirements :: [Text]
          -- ^ Security requirements.
          -- Make sense only for @OAuth@, otherwise leave empty.
        , asiDefinition :: S.SecurityScheme
          -- ^ Description of authentication method.
        }
    | NoAuthSwaggerInfo

class AuthHasSwagger auth where
    -- | Name of authentication method.
    authNameDoc :: Text
    default authNameDoc :: Typeable auth => Text
    authNameDoc = show $ typeRep (Proxy @auth)

    -- | Security requirements.
    -- Make sense only for @OAuth@, otherwise leave empty.
    authSecurityDoc :: AuthSwaggerInfo

class AuthsHaveSwagger (auths :: [*]) where
    authsSwagger :: S.Swagger

instance AuthsHaveSwagger '[] where
    authsSwagger = mempty

instance (AuthHasSwagger auth, AuthsHaveSwagger auths) =>
         AuthsHaveSwagger (auth ': auths) where
    authsSwagger = addAuth (authSecurityDoc @auth) $ authsSwagger @auths
      where
        addAuth auth sw = case auth of
            NoAuthSwaggerInfo -> sw
            AuthSwaggerInfo{..} -> sw
                & S.security <>~
                    [ S.SecurityRequirement $
                        mempty & at (authNameDoc @auth) ?~ asiRequirements
                    ]
                & S.securityDefinitions . at (authNameDoc @auth) ?~ asiDefinition

instance (HasSwagger subApi, AuthsHaveSwagger auths) =>
         HasSwagger (Auth' auths a :> subApi) where
    toSwagger _ = toSwagger (Proxy @subApi)
        & S.responses . at response401Name ?~ response401
        & S.allOperations . S.responses . S.responses . at 401 ?~
            S.Ref (S.Reference response401Name)
        & (<> authsSwagger @auths)
      where
        response401Name = "Unauthorized"
        response401 = mempty
            & S.description .~ "Unauthorized"
            & S.headers . at "WWW-Authenticate" ?~
                (mempty & S.type_ .~ S.SwaggerString)

jwtSecurityDoc :: Text -> AuthSwaggerInfo
jwtSecurityDoc desc = AuthSwaggerInfo
    { asiRequirements = []
    , asiDefinition = S.SecurityScheme
        { S._securitySchemeType = S.SecuritySchemeApiKey S.ApiKeyParams
            { S._apiKeyName = "Authorization"
            , S._apiKeyIn = S.ApiKeyHeader
            }
        , S._securitySchemeDescription = Just desc
        }
    }

educatorAuthDocDesc :: Text
educatorAuthDocDesc =
    $(embedResourceStringFile $ foldr1 (</>)
        [ "specs"
        , "disciplina"
        , "educator"
        , "api"
        , "authentication.md"
        ]
     )

-- | Top-level swagger entries and global transformations.
module Dscp.Web.Swagger.Generation.Global
    ( networkAddressToSwaggerHost
    , toAwesomeSwagger
    , setSerokellDocMeta
    , encodeSwagger
    ) where

import Control.Lens (at, ix, makePrisms, zoom, (.=), (?=))
import Data.Aeson (toEncoding)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Swagger (Swagger, URL (..))
import qualified Data.Swagger as S
import GHC.TypeLits (AppendSymbol, KnownSymbol, Symbol)
import Servant ((:<|>), (:>), Capture', Description, NoContent, QueryFlag, QueryParam', StdMethod,
                Verb)
import Servant.Swagger (HasSwagger (..))

import Dscp.Util
import Dscp.Web.Swagger.Generation.Class
import Dscp.Web.Types

makePrisms ''S.Referenced

networkAddressToSwaggerHost :: NetworkAddress -> S.Host
networkAddressToSwaggerHost addr =
    S.Host
    { S._hostName = toString $ naHost addr
    , S._hostPort = Just . fromIntegral $ naPort addr
    }

-- | Apply all meta info which is fixed for this project.
setSerokellDocMeta :: State Swagger ()
setSerokellDocMeta = do
    zoom S.info $ do
        S.termsOfService ?= "https://disciplina.io/tnc.pdf"
        S.contact ?= mempty &: do
            S.name ?= "Serokell OÜ"
            S.email ?= "hi@serokell.io"
            S.url ?= URL "https://serokell.io"
        S.license ?= "Apache 2.0" &: do
            S.url ?= URL "http://www.apache.org/licenses/LICENSE-2.0.html"

    S.externalDocs ?= mempty &: do
        S.description ?= "Find out more about Swagger"
        S.url .= URL "http://swagger.io"

-- | Cut path prefix, remain only semantically significant parts.
type family CutApiPrefix api where
    CutApiPrefix ((path :: Symbol) :> api) = CutApiPrefix api
    CutApiPrefix api = api

-- | Get path prefix.
type family ExtractApiPrefix api :: Symbol where
    ExtractApiPrefix ((path :: Symbol) :> api) =
        "/" `AppendSymbol` path `AppendSymbol` ExtractApiPrefix api
    ExtractApiPrefix api = ""

{- | This applies following transformations to API for the sake of better swagger
documentation.

* Response of methods returning `()` is replaced with `NoContents` (otherwise invalid
swagger is generated).

* `Capture`s and `QueryParam`s are attached a description according to
'ParamDescription' type family (default description is empty).

* @QueryFlag name@ occurences are attached descriptions according to
@ParamsDescription (QueryFlagDescription name)@ (there is no description by default).
-}
type family SwaggerrizeApi api where
    SwaggerrizeApi ((path :: Symbol) :> api) =
        path :> SwaggerrizeApi api

    SwaggerrizeApi (Capture' mods sym a :> api) =
        SwaggerCapture (Description (ParamDescription a) ': mods) sym a :> SwaggerrizeApi api

    SwaggerrizeApi (QueryParam' mods sym a :> api) =
        QueryParam' (Description (RequiredParamDescription mods a) ': mods) sym a
        :> SwaggerrizeApi api

    SwaggerrizeApi (QueryFlag name :> api) =
        SwaggerQueryFlag name :> SwaggerrizeApi api

    SwaggerrizeApi (arg :> api) =
        arg :> SwaggerrizeApi api

    SwaggerrizeApi (api1 :<|> api2) =
        SwaggerrizeApi api1 :<|> SwaggerrizeApi api2

    SwaggerrizeApi (Verb (method :: StdMethod) (code :: Nat) ctx ()) =
        Verb method code ctx NoContent

    SwaggerrizeApi (Verb (method :: StdMethod) (code :: Nat) ctx a) =
        Verb method code ctx a

-- | Apply some beautiness to automatically generated spec.
swaggerPostfixes :: Swagger -> Swagger
swaggerPostfixes = execState $ do
    -- Neat responses descriptions.
    zoom (S.allOperations . S.responses . S.responses) $ do
        ix 200 . _Inline . S.description .= "Successfull operation"
        ix 201 . _Inline . S.description .= "Created"
        ix 202 . _Inline . S.description .= "Accepted"

    -- Servant-swagger can mention 400 response code itself and it even enlists
    -- parameters which may cause this response.
    -- The problem is, it includes query parameters and request body, but
    -- omits path parameters ('Capture's) and it's not clear how to fix this easily,
    -- so to avoid confusion we rewrite description of 400 responses.
    zoom S.allOperations $ do
        params <- use S.parameters
        unless (null params) $
            at 400 ?= S.Inline (mempty & S.description .~ "Invalid entries format")

-- | Generate swagger documentation in a specific format we used to.
toAwesomeSwagger
    :: forall api.
       ( HasSwagger (SwaggerrizeApi (CutApiPrefix api))
       , KnownSymbol (ExtractApiPrefix api)
       )
    => Proxy api -> Swagger
toAwesomeSwagger _ =
    toSwagger (Proxy @(SwaggerrizeApi (CutApiPrefix api))) &: do
        modify swaggerPostfixes
        S.basePath ?= symbolValT @(ExtractApiPrefix api)

-- | Build a swagger documentation.
encodeSwagger :: Swagger -> LByteString
encodeSwagger =
    -- Super sad, we seem to have a little choice but to print unpretty raw JSON
    -- if we want to preserve the order of endpoints.
    -- Consider applying command-line tools to prettiy or convert the output of
    -- this function.
    encodingToLazyByteString . toEncoding

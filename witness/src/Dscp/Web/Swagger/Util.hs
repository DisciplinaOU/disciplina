-- | Utils for building swagger documentation.
module Dscp.Web.Swagger.Util
       ( -- * General
         toAwesomeSwagger
       , setSerokellDocMeta
       , encodeSwagger
       , _Inline

         -- * Utilities
       , ParamDescription
       , dscpSchemaOptions
       , setParamDescription
       , setExample
       , inDeclaredSchema
       , newtypeDeclareNamedSchema
       , gToParamSchema
       , gDeclareNamedSchema
       , idParamSchema
       , idDeclareNamedSchema
       , timestampFormat

         -- * Enum & errors documentation
       , errorDocNoDesc
       , errorCaseDocDesc
       , EnumHasDescription (..)
       , gEnumDocDesc
       , setDocEnumDescription
       ) where

import qualified Control.Exception as E
import Control.Lens (at, ix, makePrisms, zoom, (.=), (?=), _head)
import Data.Aeson (ToJSON, Value, toEncoding, toJSON)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Swagger (Schema, Swagger, ToSchema (..), URL (..))
import qualified Data.Swagger as S
import Data.Swagger.Declare (Declare)
import qualified Data.Swagger.Internal.ParamSchema as S
import qualified Data.Swagger.Internal.Schema as S
import Data.Swagger.Internal.TypeShape (GenericHasSimpleShape, GenericShape)
import qualified Data.Text as T
import Data.Typeable (typeRep)
import Fmt ((+|), (|+))
import qualified GHC.Generics as G
import GHC.TypeLits (KnownSymbol, Symbol)
import Servant ((:<|>), (:>), Capture', Description, NoContent, QueryFlag, QueryParam',
                ServantErr (..), StdMethod, Verb)
import Servant.Swagger (HasSwagger (..))

import Dscp.Util
import Dscp.Util.Aeson
import Dscp.Util.Constructors
import Dscp.Web.Class

----------------------------------------------------------------------------
-- General
----------------------------------------------------------------------------

makePrisms ''S.Referenced

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

{- | This applies following transformations to API for the sake of better swagger
documentation.

* Response of methods returning `()` is replaced with `NoContents` (otherwise invalid
swagger is generated).

* `Capture`s and `QueryParam`s are attached a description according to
'ParamDescription' type family.

* @QueryFlag name@ occurences are attached descriptions according to
@ParamsDescription (QueryFlagDescription name)@
-}
type family SwaggerrizeApi api where
    SwaggerrizeApi ((path :: Symbol) :> api) =
        path :> SwaggerrizeApi api

    SwaggerrizeApi (Capture' mods sym a :> api) =
        Capture' (Description (ParamDescription a) ': mods) sym a :> SwaggerrizeApi api

    SwaggerrizeApi (QueryParam' mods sym a :> api) =
        QueryParam' (Description (ParamDescription a) ': mods) sym a :> SwaggerrizeApi api

    SwaggerrizeApi (QueryFlag name :> api) =
        QueryFlagParam name :> SwaggerrizeApi api

    SwaggerrizeApi (arg :> api) =
        arg :> SwaggerrizeApi api

    SwaggerrizeApi (api1 :<|> api2) =
        SwaggerrizeApi api1 :<|> SwaggerrizeApi api2

    SwaggerrizeApi (Verb (method :: StdMethod) (code :: Nat) ctx ()) =
        Verb method code ctx NoContent

    SwaggerrizeApi (Verb (method :: StdMethod) (code :: Nat) ctx a) =
        Verb method code ctx a

-- | Description of parameter.
-- This is what you would want to refer to when defining 'ToParamSchema' and 'ToSchema'
-- instances, also see 'setParamDescription'.
type family ParamDescription p :: Symbol

-- | Replacement for 'QueryFlag' which has a better documentation.
data QueryFlagParam (name :: Symbol)

-- | Description of a given `QueryFlag` parameter.
type family QueryFlagDescription (name :: Symbol) :: Symbol

type instance QueryFlagDescription "onlyCount" =
    "If this parameter is present, return only the total count of items."

instance (HasSwagger subApi, KnownSymbol name, KnownSymbol (QueryFlagDescription name)) =>
         HasSwagger (QueryFlagParam name :> subApi) where
    toSwagger _ = toSwagger (Proxy @(QueryFlag name :> subApi)) &: do
        zoom (S.allOperations . S.parameters . _head . _Inline) $ do
            paramName <- use S.name
            E.assert (name == paramName) pass
            S.description ?= desc
      where
        name = symbolValT @name
        desc = symbolValT @(QueryFlagDescription name)

-- | Apply some beautiness to automatically generated spec.
swaggerPostfixes :: Swagger -> Swagger
swaggerPostfixes = execState $ do
    -- Neat responses descriptions.
    zoom (S.allOperations . S.responses . S.responses) $ do
        ix 200 . _Inline . S.description .= "Successfull operation."
        ix 201 . _Inline . S.description .= "Created."
        ix 202 . _Inline . S.description .= "Accepted."

    -- Servant-swagger can mention 400 response code itself and it even enlists
    -- parameters which may cause this response.
    -- The problem is, it includes query parameters and request body, but
    -- omits path parameters ('Capture's) and it's not clear how to fix this easily,
    -- so to avoid confusion we rewrite description of 400 responses.
    zoom S.allOperations $ do
        params <- use S.parameters
        unless (null params) $
            at 400 ?= S.Inline (mempty & S.description .~ "Invalid entries format.")

-- | Generate swagger documentation in a specific format we used to.
toAwesomeSwagger
    :: forall api.
       HasSwagger (SwaggerrizeApi api)
    => Proxy api -> Swagger
toAwesomeSwagger _ = swaggerPostfixes $ toSwagger (Proxy @(SwaggerrizeApi api))

-- | Build a swagger documentation.
encodeSwagger :: Swagger -> LByteString
encodeSwagger =
    -- Super sad, we seem to have a little choice but to print unpretty raw JSON
    -- if we want to preserve the order of endpoints.
    -- Consider applying command-line tools to prettiy or convert the output of
    -- this function.
    encodingToLazyByteString . toEncoding

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | Swagger schema options which correspond to our 'dscpAesonOptions'.
dscpSchemaOptions :: S.SchemaOptions
dscpSchemaOptions = S.fromAesonOptions dscpAesonOptions

-- | Set description according to 'ParamDescription' definition.
setParamDescription
    :: forall a proxy.
       KnownSymbol (ParamDescription a)
    => proxy a -> State Schema ()
setParamDescription _ = S.description ?= symbolValT @(ParamDescription a)

-- | Set the given item as example.
setExample :: (S.HasExample s (Maybe Value), ToJSON a) => a -> State s ()
setExample item = S.example ?= toJSON item

-- | Modify the schema in an implementation of 'ToSchema'.
inDeclaredSchema
    :: Declare (S.Definitions S.Schema) S.NamedSchema
    -> State Schema ()
    -> Declare (S.Definitions S.Schema) S.NamedSchema
inDeclaredSchema decl f = fmap (S.schema %~ execState f) decl

-- | Implement 'ToSchema' for the given newtype delegeting to the schema
-- of the inner type. You will still need to provide a description via 'ParamDescription'
-- type family.
newtypeDeclareNamedSchema
    :: forall a nt proxy. (ToSchema a, Coercible a nt, KnownSymbol (ParamDescription nt))
    => proxy nt -> Declare (S.Definitions S.Schema) S.NamedSchema
newtypeDeclareNamedSchema pnt = do
    inDeclaredSchema (declareNamedSchema (Proxy @a)) (setParamDescription pnt)

-- | Default implementation of 'ToParamSchema' via Generics.
gToParamSchema
    :: (Generic a, S.GToParamSchema (G.Rep a))
    => proxy a -> S.ParamSchema t
gToParamSchema = S.genericToParamSchema dscpSchemaOptions

-- | Default implementation of 'ToSchema' via Generics.
gDeclareNamedSchema
    :: ( Generic a
       , S.GToSchema (G.Rep a)
       , GenericHasSimpleShape a "genericDeclareNamedSchemaUnrestricted" (GenericShape (G.Rep a))
       )
    => proxy a -> Declare (S.Definitions S.Schema) S.NamedSchema
gDeclareNamedSchema = S.genericDeclareNamedSchema dscpSchemaOptions

-- | Implementation of 'toParamSchema' for identifiers.
idParamSchema :: S.ParamSchema t
idParamSchema = mempty &: do
    S.type_ .= S.SwaggerInteger
    S.minimum_ ?= 0
    S.maximum_ ?= fromIntegral (maxBound @Word64)

-- | Implementation of 'declareNamedSchema' for identifiers.
idDeclareNamedSchema
    :: forall a proxy.
       (Typeable a, KnownSymbol (ParamDescription a))
    => proxy a -> Declare (S.Definitions S.Schema) S.NamedSchema
idDeclareNamedSchema pa =
    return . S.named (show $ typeRep pa) $ mempty &: do
        setParamDescription pa

timestampFormat :: IsString s => s
timestampFormat = "yyyy-mm-ddThh:MM:ss.ffffffZ"

----------------------------------------------------------------------------
-- Enum descriptions
----------------------------------------------------------------------------

-- | Description of one enum value case.
data EnumDescriptionItem = EnumDescriptionItem
    { ediTag      :: Text
    , ediHttpCode :: Int
    , ediDesc     :: Maybe Text
    }

-- | Set error description without detailed explanation.
errorDocNoDesc :: ToServantErr err => err -> EnumDescriptionItem
errorDocNoDesc err = EnumDescriptionItem
    { ediTag = errorTag err
    , ediHttpCode = errHTTPCode $ toServantErrNoBody err
    , ediDesc = Nothing
    }

-- | Implementation of 'enumDocDescription' which describes single
-- error case.
errorCaseDocDesc
    :: forall fill err.
       (CanEnlistConstructors fill err, ToServantErr err, HasCallStack)
    => Proxy err -> (err -> Text) -> [EnumDescriptionItem]
errorCaseDocDesc _ mkDesc =
    enlistConstructors @fill @err <&> \err ->
        EnumDescriptionItem
        { ediTag = errorTag err
        , ediHttpCode = errHTTPCode $ toServantErrNoBody err
        , ediDesc = Just (mkDesc err)
        }

class EnumHasDescription err where
    -- | Description of every single error which is part of the given exception datatype.
    enumDocDescription :: Proxy err -> [EnumDescriptionItem]

-- | Implementation of 'enumDocDescription' where for each possible constructor
-- you specify description of the error this constructor carries.
gEnumDocDesc
    :: forall err.
       (CanEnlistConstructors UnsafeFiller err, HasCallStack)
    => (err -> [EnumDescriptionItem]) -> Proxy err -> [EnumDescriptionItem]
gEnumDocDesc mkDesc _ =
    mconcat $ map mkDesc (enlistConstructors @UnsafeFiller @err)

-- | Set description of a type which looks like enum in Swagger.
setDocEnumDescription
    :: forall err.
       (EnumHasDescription err)
    => State S.Schema ()
setDocEnumDescription = do
    let descs = dropInternalErrors $ enumDocDescription (Proxy @err)
    let desc = T.unlines $ map fmt descs
    S.description ?= desc
    S.enum_ ?= map (toJSON . ediTag) descs
  where
    dropInternalErrors = filter (\d -> ediHttpCode d /= 500)
    fmt EnumDescriptionItem{..} = mconcat
        [ "* `" +| ediTag |+ "`"
        , case ediDesc of
            Nothing -> ""
            Just d  -> ": " +| d |+ ""
        , " (" +| ediHttpCode |+ " code)"
        ]

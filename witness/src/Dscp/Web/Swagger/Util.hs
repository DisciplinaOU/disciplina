-- | Utils for building swagger documentation.
module Dscp.Web.Swagger.Util
       ( -- * General
         toAwesomeSwagger
       , setSerokellDocMeta
       , encodeSwagger
       , _Inline

         -- * Utilities
       , ParamDescription
       , QueryFlagDescription
       , FilterParam
       , FilterParamSince
       , dscpSchemaOptions
       , setParamDescription
       , setExample
       , declareSimpleSchema
       , inDeclaredSchema
       , newtypeDeclareNamedSchema
       , gToParamSchema
       , gDeclareNamedSchema
       , idParamSchema
       , idDeclareNamedSchema
       , timestampFormat
       , errResponseSchema

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
import GHC.TypeLits (AppendSymbol, KnownSymbol, Symbol)
import Servant ((:<|>), (:>), Capture', Description, NoContent, Optional, QueryFlag, QueryParam',
                ServantErr (..), StdMethod, Strict, Verb)
import Servant.Swagger (HasSwagger (..))

import Dscp.Core
import Dscp.Util
import Dscp.Util.Aeson
import Dscp.Util.Constructors
import Dscp.Web.Class

----------------------------------------------------------------------------
-- Global swagger options
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

-- | Cut path prefix, remain only semantically significant parts.
type family CutApiPrefix api where
    CutApiPrefix ((path :: Symbol) :> api) = CutApiPrefix api
    CutApiPrefix api = api

-- | Get path prefix.
type family ExtractApiPrefix api :: Symbol where
    ExtractApiPrefix ((path :: Symbol) :> api) =
        "/" `AppendSymbol` path `AppendSymbol` ExtractApiPrefix api
    ExtractApiPrefix api = ""

-- | Description of parameter.
--
-- When defining 'ToSchema' instances you have to refer to this type family manually using
-- 'setParamDescription' function; 'ToParamSchema' instances does not allow specifying
-- a description and this is done automatically at a higher level (via 'SwaggerrizeApi').
type family ParamDescription p :: Symbol

-- | Replacement for 'QueryFlag' which has a better documentation.
data SwaggerQueryFlag (name :: Symbol)

-- | Defines swagger description for the given `QueryFlag` parameter.
type family QueryFlagDescription (name :: Symbol) :: Symbol

type instance QueryFlagDescription "onlyCount" =
    "If this parameter is present, return only the total count of items."

instance (HasSwagger subApi, KnownSymbol name, KnownSymbol (QueryFlagDescription name)) =>
         HasSwagger (SwaggerQueryFlag name :> subApi) where
    toSwagger _ = toSwagger (Proxy @(QueryFlag name :> subApi)) &: do
        zoom (S.allOperations . S.parameters . _head . _Inline) $ do
            paramName <- use S.name
            E.assert (name == paramName) pass
            S.description ?= desc
      where
        name = symbolValT @name
        desc = symbolValT @(QueryFlagDescription name)

-- | Like 'Capture', but does not modify description of 404 error (which looks
-- pretty like robot-generated).
data SwaggerCapture (mods :: [*]) (sym :: Symbol) a

instance (HasSwagger (Capture' mods sym a :> api), HasSwagger api) =>
         HasSwagger (SwaggerCapture mods sym a :> api) where
    toSwagger _ =
        toSwagger (Proxy @(Capture' mods sym a :> api)) &: do
            desc404L .= fromMaybe "" pureDesc404
      where
        desc404L :: Traversal' Swagger Text
        desc404L = S.allOperations . S.responses . S.responses .
                   ix 404 . _Inline . S.description
        pureDesc404 = toSwagger (Proxy @api) ^? desc404L

-- | Type of filtering parameter.
data FilterParamType
    = MatchingFilter
      -- ^ Exact value match.
    | SinceFilter
      -- ^ Timestamp is greater than a given one.

-- | Modifier for 'QueryParam\'', it effectively affects only swagger documentation.
data FilterParamMod (ft :: FilterParamType)

-- | Behaves like 'QueryParam', but documentation will mention that provided value
-- is used as filter.
type FilterParamExt ft = QueryParam' [Optional, Strict, FilterParamMod ft]

type FilterParam = FilterParamExt 'MatchingFilter
type FilterParamSince = FilterParamExt 'SinceFilter

-- | Wrapper which changes 'ParamDescription' type instance for a type so that
-- it mentions type's purpose (being a filter).
data MentionAsFilter (ft :: FilterParamType) a

type instance ParamDescription (MentionAsFilter 'MatchingFilter a) =
    "Filter result items by given value - " `AppendSymbol` ParamDescription a

type instance ParamDescription (MentionAsFilter 'SinceFilter Timestamp) =
    "Return only items starting with the given time."

-- | Gets some version of 'ParamDescription' depending on the given modificators.
type family RequiredParamDescription mods a where
    RequiredParamDescription '[] a =
        ParamDescription a

    RequiredParamDescription (FilterParamMod ft ': ms) a =
        RequiredParamDescription ms (MentionAsFilter ft a)

    RequiredParamDescription (m ': ms) a =
        RequiredParamDescription ms a

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

-- | The most straitforward implementation of 'ToSchema'.
declareSimpleSchema :: Text -> Schema -> Declare (S.Definitions S.Schema) S.NamedSchema
declareSimpleSchema name schema = return $ S.named name schema

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
       ( Typeable a
       , S.ToParamSchema a
       , KnownSymbol (ParamDescription a)
       )
    => proxy a -> Declare (S.Definitions S.Schema) S.NamedSchema
idDeclareNamedSchema pa =
    declareSimpleSchema (show $ typeRep pa) $ (S.paramSchemaToSchema pa) &: do
        setParamDescription pa

timestampFormat :: IsString s => s
timestampFormat = "yyyy-mm-ddThh:MM:ss.ffffffZ"

-- | Template for error schema (corresponding to `ErrResponse`).
errResponseSchema :: State Schema () -> Schema
errResponseSchema errTagSchemaModifier = mempty &: do
    S.type_ .= S.SwaggerObject
    S.required .= ["error"]
    zoom S.properties $ do
        at "error" ?= errSchema
    S.description ?= "Describes a respose error."
  where
    errSchema = S.toSchemaRef (Proxy @String) &: do
        zoom _Inline $ errTagSchemaModifier

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

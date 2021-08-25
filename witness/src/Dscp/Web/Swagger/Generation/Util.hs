-- | Helpers for defining swagger instances.
module Dscp.Web.Swagger.Generation.Util
    ( _Inline
    , dscpSchemaOptions
    , setExample
    , declareSimpleSchema
    , inDeclaredSchema
    , newtypeDeclareNamedSchema
    , gToParamSchema
    , gDeclareNamedSchema
    , idParamSchema
    , idDeclareNamedSchema
    , errResponseSchema
    ) where

import Control.Lens (at, makePrisms, zoom, (.=), (?=))
import Data.Aeson (ToJSON, Value, toJSON)
import Data.Swagger (Schema, ToSchema (..))
import qualified Data.Swagger as S
import Data.Swagger.Declare (Declare)
import qualified Data.Swagger.Internal.ParamSchema as S
import qualified Data.Swagger.Internal.Schema as S
import Data.Swagger.Internal.TypeShape (GenericHasSimpleShape, GenericShape)
import Data.Typeable (typeRep)
import qualified GHC.Generics as G
import GHC.TypeLits (KnownSymbol)
import Servant.Util.Swagger (ParamDescription, paramDescription)

import Dscp.Util
import Dscp.Util.Aeson


makePrisms ''S.Referenced

-- | Swagger schema options which correspond to our 'dscpAesonOptions'.
dscpSchemaOptions :: S.SchemaOptions
dscpSchemaOptions = S.fromAesonOptions dscpAesonOptions

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
    inDeclaredSchema (declareNamedSchema (Proxy @a)) $
        S.description ?= paramDescription pnt

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
        S.description ?= paramDescription pa

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

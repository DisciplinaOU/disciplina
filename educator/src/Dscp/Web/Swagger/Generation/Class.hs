-- | All typeclasses and type families we rely on when deriving swagger spec.
module Dscp.Web.Swagger.Generation.Class
    ( ParamDescription
    , RequiredParamDescription
    , SwaggerCapture
    , QueryFlagDescription
    , SwaggerQueryFlag
    , FilterParam
    , FilterParamSince
    ) where

import Universum

import qualified Control.Exception as E
import Control.Lens (ix, makePrisms, zoom, (.=), (?=), _head)
import Data.Swagger (Swagger)
import qualified Data.Swagger as S
import GHC.TypeLits (AppendSymbol, KnownSymbol, Symbol)
import Servant ((:>), Capture', Optional, QueryFlag, QueryParam', Strict)
import Servant.Swagger (HasSwagger (..))

import Dscp.Core
import Dscp.Util

makePrisms ''S.Referenced

----------------------------------------------------------------------------
-- Parameter description
----------------------------------------------------------------------------

-- | Description of parameter.
--
-- When defining 'ToSchema' instances you have to refer to this type family manually using
-- 'setParamDescription' function; 'ToParamSchema' instances does not allow specifying
-- a description and this is done automatically at a higher level (via 'SwaggerrizeApi').
type family ParamDescription p :: Symbol

----------------------------------------------------------------------------
-- Query flag description
----------------------------------------------------------------------------

-- | Defines swagger description for the given `QueryFlag` parameter.
type family QueryFlagDescription (name :: Symbol) :: Symbol

-- | Replacement for 'QueryFlag' which has a better documentation.
data SwaggerQueryFlag (name :: Symbol)

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

----------------------------------------------------------------------------
-- Capture description
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- Filter query parameters
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- Query parameters - overall
----------------------------------------------------------------------------

-- | Gets some version of 'ParamDescription' depending on the given modificators.
type family RequiredParamDescription mods a where
    RequiredParamDescription '[] a =
        ParamDescription a

    RequiredParamDescription (FilterParamMod ft ': ms) a =
        RequiredParamDescription ms (MentionAsFilter ft a)

    RequiredParamDescription (m ': ms) a =
        RequiredParamDescription ms a

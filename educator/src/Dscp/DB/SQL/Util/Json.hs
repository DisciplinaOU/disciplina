{- | Provides means of safe JSON fields access in a database.

Primarily intended for product datatypes with automatically derived
'Aeson.ToJSON' and 'Aeson.FromJSON' instances which appear in Postgres database
schema.
-}

module Dscp.DB.SQL.Util.Json
    ( JsonFieldType
    , jsonFieldName
    , HasSqlCastableJson
    , (->$.)
    , (->>$.)
    ) where

import qualified Data.Aeson as Aeson hiding (defaultOptions)
import qualified Data.Aeson.Options as Aeson
import Data.Time.Calendar (Day)
import Database.Beam.Migrate (HasDefaultSqlDataType (..))
import Database.Beam.Postgres (IsPgJSON, (->$), (->>$))
import Database.Beam.Postgres.Syntax (PgDataTypeSyntax, PgExpressionSyntax)
import Database.Beam.Query (QGenExpr, val_)
import qualified GHC.Generics as G
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)

import Dscp.Core
import Dscp.DB.SQL.Util.Common
import Dscp.Util.Type

----------------------------------------------------------------------------
-- API
----------------------------------------------------------------------------

-- | Safely get a JSON field name.
-- You need to provide your type, name of the field (via type-level annotations)
-- and options used for automatic derivation.
jsonFieldName
    :: forall a (fname :: Symbol).
       HasJsonField a fname
    => Aeson.Options -> Text
jsonFieldName = gJsonFieldName @a @fname @(G.Rep a)

type HasJsonField a name = (G.Generic a, GHasJsonField a name (G.Rep a))

-- | Type of the field with the given name.
type JsonFieldType a name = GJsonFieldType name (G.Rep a)


-- | Instantiating this typeclass you say that textual JSON representation
-- of any object of a given type can be safely casted back to the given type
-- within Postgres using @cast@ SQL operator.
class HasSqlCastableJson a

-- | Safely access a JSON field which is itself a JSON.
-- Assumes that Aeson instances for the given type are derived
-- with 'Aeson.defaultOptions'.
(->$.)
    :: forall fname a json ctx s.
       (IsPgJSON json, HasJsonField a fname)
    => QGenExpr ctx PgExpressionSyntax s (json a)
    -> Proxy fname
    -> QGenExpr ctx PgExpressionSyntax s (json (JsonFieldType a fname))
json ->$. _ = json ->$ val_ (jsonFieldName @a @fname Aeson.defaultOptions)
infixr 8 ->$.

-- | Safely access a JSON field of a primitive type.
-- Assumes that Aeson instances for the given type are derived
-- with 'Aeson.defaultOptions'.
(->>$.)
    :: forall fname a json ctx s.
       ( IsPgJSON json, HasJsonField a fname
       , HasDefaultSqlDataType PgDataTypeSyntax (JsonFieldType a fname)
       , HasSqlCastableJson (JsonFieldType a fname)
       )
    => QGenExpr ctx PgExpressionSyntax s (json a)
    -> Proxy fname
    -> QGenExpr ctx PgExpressionSyntax s (JsonFieldType a fname)
json ->>$. _ =
    unsafeCast_ $ json ->>$ val_ (jsonFieldName @a @fname Aeson.defaultOptions)
infixr 9 ->>$.

----------------------------------------------------------------------------
-- Base instances
----------------------------------------------------------------------------

instance HasSqlCastableJson Int
instance HasSqlCastableJson Day
instance HasSqlCastableJson ItemDesc
instance HasSqlCastableJson Timestamp

----------------------------------------------------------------------------
-- Internals
----------------------------------------------------------------------------

-- | What you get when requested field does not exist in your datatype.
data FieldNotFound

-- | Safely access json field.
class GHasJsonField orig (name :: Symbol) (x :: * -> *) where
    -- | Type of the accessed field.
    type GJsonFieldType name x :: *

    -- | Get name of the field as it is encoded in JSON.
    gJsonFieldName :: Aeson.Options -> Text

instance TypeError ('Text "Lol no, sum types are not supported") =>
         GHasJsonField orig name (G.D1 dm (c1 G.:+: c2)) where

    type GJsonFieldType name (G.D1 dm (c1 G.:+: c2)) =
        TypeError ('Text "Cannot access sum type")

    gJsonFieldName =
        error "no way"

instance TypeError ('Text "Constructor has no fields") =>
         GHasJsonField orig name (G.D1 dm (G.C1 cm G.U1)) where

    type GJsonFieldType name (G.D1 dm (G.C1 cm G.U1)) =
        TypeError ('Text "Constructor has no fields")

    gJsonFieldName =
        error "nothing is here"

instance TypeError ('Text "Accessing constructors with one field \
                          \is not yet implemented") =>
         GHasJsonField orig name (G.D1 dm (G.C1 cm (G.S1 sm (G.Rec0 inner)))) where

    type GJsonFieldType name (G.D1 dm (G.C1 cm (G.S1 sm (G.Rec0 inner)))) =
        TypeError ('Text "Cannot yet access constructor with only one field")

    gJsonFieldName =
        -- Rules for deriving JSON field in this case are a bit
        -- more complex than usual.
        error "maybe some day..."

instance ( KnownSymbol name
         , GProductHasJsonField orig name (s1 G.:*: s2)
         ) =>
         GHasJsonField orig name (G.D1 dm (G.C1 cm (s1 G.:*: s2))) where

    type GJsonFieldType name (G.D1 dm (G.C1 cm (s1 G.:*: s2))) =
        GProductJsonFieldType name (s1 G.:*: s2)

    gJsonFieldName opts =
        {- Note on optional fields:
          We do not actually care about whether 'Aeson.omitNothingFields' option is set
          or not, because Postgres treats "null" fields and absent fields equally - they
          are SQL nulls.
          For instance, this is good for our sorting, because fields which are "Maybe"
          can be justifiably sorted with nulls-first/nulls-last option specified.
        -}
        toText $ Aeson.fieldLabelModifier opts (symbolVal (Proxy @name))

-- | Access a JSON field in product type.
type family GProductJsonFieldType name (x :: * -> *) :: * where

    GProductJsonFieldType name (x G.:*: y) =
        If (GProductJsonFieldType name x == FieldNotFound)
           (GProductJsonFieldType name y)
           (GProductJsonFieldType name x)

    GProductJsonFieldType name (G.S1 ('G.MetaSel ('Just fname) u s l) (G.Rec0 ty))
        = If (name == fname) ty FieldNotFound

    GProductJsonFieldType name (G.S1 ('G.MetaSel 'Nothing u s l) (G.Rec0 ty))
        = FieldNotFound

-- | Whether a field is present in a type product.
type family GProductHasJsonField orig name x :: Constraint where
    GProductHasJsonField orig name x =
        If (GProductJsonFieldType name x == FieldNotFound)
            (TypeError ('Text "No field " ':<>: 'ShowType name ':<>:
                        'Text " in " ':<>: 'ShowType orig))
            (() :: Constraint)

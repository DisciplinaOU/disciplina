-- | Utils at the junction of Servant API and Beam DSL.
module Dscp.DB.SQLite.ApiUtil
    ( SortingSpecApp
    , fieldSort_
    , bySpec_

      -- * Re-exports
    , HList (..)
    , (.*.)
    , Sql92OrderingExpressionSyntax
    , IsSql92OrderingSyntax
    ) where

import Data.Coerce (coerce)
import Data.HList (HList (..), (.*.))
import Database.Beam.Backend.SQL.SQL92 (IsSql92OrderingSyntax, Sql92OrderingExpressionSyntax)
import Database.Beam.Query.Internal (QExpr, QOrd)
import GHC.TypeLits (KnownSymbol)

import Dscp.DB.SQLite.Util
import Dscp.Util
import Dscp.Util.Servant

-- | Under the hood we don't really care about the type we are sorting on.
type SomeQOrd syntax s = QOrd syntax s Void

-- | A function defining a way to apply the given 'SortingItem' (which is sorting
-- order on a single parameter) as a part of Beam's 'orderBy_'.
type SortingToBeam name syntax s a =
    SortingItemTagged ('TyNamedParam name a) -> SomeQOrd syntax s

-- | Implement 'SortingToBeam' as sorting on the given table field.
fieldSort_
    :: forall name a syntax s.
       (IsSql92OrderingSyntax syntax)
    => QExpr (Sql92OrderingExpressionSyntax syntax) s a
    -> SortingToBeam name syntax s a
fieldSort_ field (SortingItemTagged SortingItem{..}) = order (coerce field)
  where
    order = case siOrder of
        Ascendant  -> asc_
        Descendant -> desc_

    -- TODO [DSCP-425]
    -- Ordering NULLs is not supported by SQLite :peka:
    -- nullsOrder = case siNullsOrder of
    --     Nothing         -> id
    --     Just NullsFirst -> nullsFirst_
    --     Just NullsLast  -> nullsLast_

-- | Maps each named parameter to 'SortingToBeam'.
type family SqlOrderingApp (params :: [TyNamedParam *]) syntax s :: [*] where
    SqlOrderingApp '[] syntax s = '[]
    SqlOrderingApp ('TyNamedParam name a ': params) syntax s =
        SortingToBeam name syntax s a ': SqlOrderingApp params syntax s

{- | List of 'SortingToBeam' functions. Describes how to apply @SortingSpec params@
to an SQL query.

Instance of this type can be created using 'fieldSort_' function. For example:

@
let defSortingSpecApp :: SortingSpecApp ["course" ?: Course, "desc" ?: Text]
    defSortingSpecApp =
        fieldSort_ @"course" courseField .*.
        fieldSort_ @"desc" descField .*.
        HNil
@

Annotating 'fieldSort_' call with parameter name is not mandatory but recommended
to prevent possible mistakes in 'fieldSort_'s ordering.
-}
type SortingSpecApp params syntax s = HList (SqlOrderingApp params syntax s)

-- | Lookup for appropriate 'SortingToBeam' in 'SortingSpecApp' and apply it to 'SortingItem'.
class ApplyToSortItem params syntax s where
    -- | Apply spec app to the given 'SortingItem'
    -- We return 'Maybe' here (instead of forcing presence at type-level) for convenience.
    applyToSortItem
        :: SortingSpecApp params syntax s
        -> SortingItem
        -> Maybe (SomeQOrd syntax s)

instance ApplyToSortItem '[] syntax s where
    applyToSortItem HNil _ = Nothing

instance (KnownSymbol name, ApplyToSortItem params syntax s) =>
         ApplyToSortItem ('TyNamedParam name p ': params) syntax s where
    applyToSortItem (app `HCons` appRem) item = asum
        [ guard (symbolValT @name == siName item) $> app (SortingItemTagged item)
        , applyToSortItem @params @syntax @s appRem item
        ]

-- | Apply a given 'SortingSpecApp' to a 'SortingSpec' producing a value which can be put
-- to 'orderBy_'.
bySpec_
    :: forall params syntax s.
       ApplyToSortItem params syntax s
    => SortingSpec params
    -> SortingSpecApp params syntax s
    -> [SomeQOrd syntax s]
bySpec_ spec app =
    unSortingSpec spec <&> \sitem ->
        applyToSortItem @params @syntax @s app sitem
           -- impossible due to invariants of 'SortingSpec'
        ?: error ("Impossible: don't know how to apply to spec item " <> show sitem)

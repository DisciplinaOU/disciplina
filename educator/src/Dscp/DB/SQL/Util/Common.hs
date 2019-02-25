{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Utilities for writing SQL queries.

module Dscp.DB.SQL.Util.Common
     ( module BeamReexport
     , checkExists
     , insertValue
     , insertExpression
     , pk_
     , PrimaryKeyWrapper
     , packPk
     , unpackPk
     , valPk_
     , selectByPk
     , existsWithPk
     , deleteByPk
     , coerceQExpr
     , currentTimestampUtc_
     , filterMatches_
     , filterMatchesPk_
     , getNextPrimaryKey
     , unsafeCast_
     ) where

import Prelude hiding (_1, _2)

import Data.Coerce (coerce)
import qualified Database.Beam.Backend.SQL as Beam
import Database.Beam.Migrate (HasDefaultSqlDataType (..))
import Database.Beam.Postgres as BeamReexport (PgJSONB (..))
import qualified Database.Beam.Postgres as Beam
import Database.Beam.Query as BeamReexport (QExpr, QGenExpr (..), aggregate_, all_, as_, asc_,
                                            countAll_, default_, delete, desc_, exists_, filter_,
                                            guard_, in_, insert, insertValues, leftJoin_, limit_, max_,
                                            orderBy_, references_, related_, select, update, val_,
                                            (&&.), (/=.), (<-.), (==.), (>.), (>=.), (||.))
import qualified Database.Beam.Query as Beam
import qualified Database.Beam.Query.Internal as Beam
import Database.Beam.Schema (PrimaryKey, TableEntity)
import Database.Beam.Schema as BeamReexport (DatabaseSettings)
import qualified Database.Beam.Schema as Beam
import qualified GHC.Generics as G
import GHC.TypeLits (ErrorMessage (Text), TypeError)

import Dscp.Core
import Dscp.DB.SQL.Functions
import Dscp.Util

-- | Check whether the query returns any row.
checkExists
    :: (MonadIO m)
    => Beam.Q Beam.PgSelectSyntax db (Beam.QNested _) () -> DBT t m Bool
checkExists query =
    fmap ((> 0) . oneOrError) $
    runSelect . select $
    aggregate_ (\_ -> countAll_) (query $> as_ @Int 1)

-- | Build a 'SqlInsertValues' from concrete table value.
insertValue :: _ => table Identity -> Beam.SqlInsertValues syntax (table (Beam.QExpr _ s))
insertValue = insertValues . one

-- | Build a 'SqlInsertValues' from concrete table value.
insertExpression
    :: _
    => (forall s'. table (Beam.QExpr _ s'))
    -> Beam.SqlInsertValues syntax (table (Beam.QExpr _ s))
insertExpression expr = Beam.insertExpressions (one expr)

-- | Lift a value to primary key.
-- @martoon: I prefer remaining "pk" for variable name, thus reexporting modified.
pk_ :: Beam.Table table => table a -> PrimaryKey table a
pk_ = Beam.pk

-- | Class-helper to wrap or unwrap constructors of 'PrimaryKey' in generic way.
class GPrimaryKeyWrapper (pk :: * -> *) inner where
    gUnpackPk :: pk p -> inner
    gPackPk :: inner -> pk p

instance (inner ~ inner') =>
         GPrimaryKeyWrapper (G.D1 _d (G.C1 _c (G.S1 s' (G.Rec0 inner)))) inner' where
    gUnpackPk = coerce
    gPackPk = coerce

instance TypeError ('Text "Cannot wrap/unwrap primary key containing several values") =>
         GPrimaryKeyWrapper (G.D1 _d (G.C1 _c (s1 G.:*: s2))) inner where
    gUnpackPk = error "impossible"
    gPackPk = error "impossible"

instance TypeError ('Text "Cannot wrap/unwrap for empty primary key") =>
         GPrimaryKeyWrapper (G.D1 _d (G.C1 _c G.U1)) inner where
    gUnpackPk = error "impossible"
    gPackPk = error "impossible"

type PrimaryKeyWrapper pk inner = (Generic pk, GPrimaryKeyWrapper (G.Rep pk) inner)

-- | Lift an entity to primary key, effectively just wrappes it into 'PrimaryKey' constructor.
-- Works only for primary keys which consists of one item.
packPk :: PrimaryKeyWrapper pk inner => inner -> pk
packPk = G.to . gPackPk

-- | Unwrap 'PrimaryKey' constructor.
-- Works only for primary keys which consists of one item.
unpackPk :: PrimaryKeyWrapper pk inner => pk -> inner
unpackPk = gUnpackPk . G.from

-- | Wrap an entiry into a primary key sql value.
valPk_
    :: (Beam.SqlValable pk, PrimaryKeyWrapper (Beam.HaskellLiteralForQExpr pk) inner)
    => inner -> pk
valPk_ = val_ . packPk

-- | Quick way to fetch a single entiry refered by the given primary key.
selectByPk
    :: (MonadIO m, HasCallStack, _)
    => (row Identity -> res)
    -> Beam.DatabaseEntity be db (TableEntity table)
    -> pk
    -> DBT t m (Maybe res)
selectByPk mapper tbl key =
    fmap fetchOne $
    runSelect . Beam.select $
    filter_ (valPk_ key `references_`) $
    all_ tbl
  where
    fetchOne [] = Nothing
    fetchOne l  = Just (mapper $ oneOrError l)

-- | Quick way to check whether an entiry refered by the given primary key exists.
existsWithPk
    :: _
    => Beam.DatabaseEntity be db (TableEntity table) -> pk -> DBT t m Bool
existsWithPk tbl key =
    checkExists $ do
        row <- all_ tbl
        guard_ (Beam.pk row ==. valPk_ key)

-- | Quick way to delete a single entiry refered by the given primary key.
-- Returns whether anything was actually deleted.
deleteByPk
    :: (MonadIO m, _)
    => Beam.DatabaseEntity be db (TableEntity table)
    -> pk
    -> DBT t m Bool
deleteByPk tbl key = do
    changes <- runDelete $ delete tbl (valPk_ key `references_`)
    return (anyAffected changes)

-- | Safely coerce one Beam SQL expression to another assuming that
-- both expressions are represented in SQL engine in the same way.
coerceQExpr :: Coercible a b => QGenExpr ctx syntax s a -> QGenExpr ctx syntax s b
coerceQExpr = coerce

-- | SQL CURRENT_TIMESTAMP function.
currentTimestampUtc_
    :: forall ctxt syntax s.
       Beam.IsSql92ExpressionSyntax syntax
    => Beam.QGenExpr ctxt syntax s Timestamp
currentTimestampUtc_ =
    -- The behavior should not depend much on the backend.
    -- CURRENT_TIMESTAMP is nicely fetched both for "datetime" and "timestamp" types,
    -- and returned value should already have no more than microseconds precision.
    coerce (Beam.currentTimestamp_ @syntax)

-- | Make a filtering predicate checking for values match.
filterMatches_
    :: _
    => Maybe a -> QGenExpr syntax ctx s a -> QGenExpr syntax ctx s Bool
filterMatches_ = \case
    Nothing -> \_ -> val_ True
    Just x  -> (==. val_ x)

-- | Make a filtering predicate checking for values match.
filterMatchesPk_
    :: _
    => Maybe a
    -> PrimaryKey row (QGenExpr syntax ctx s)
    -> QGenExpr syntax ctx s Bool
filterMatchesPk_ = \case
    Nothing -> \_ -> val_ True
    Just x  -> (==. valPk_ x)

-- | Take the next primary key for the given table, can be used for further insert.
-- In [DSCP-388] we probably remove this function for the sake of specialized methods
-- to do the similar thing.
-- We assume that primary key is exactly of "bigserial" type, thus an extra
-- 'Coercible' constraint here.
getNextPrimaryKey
    :: ( PrimaryKeyWrapper (PrimaryKey table Identity) keyInner
       , Num keyInner
       , MonadIO m
       , Beam.Table table
       , Beam.Database be db
       , Beam.FromBackendRow Beam.Postgres keyInner
       , Coercible keyInner Int64
       , _
       )
    => Beam.DatabaseEntity Beam.Postgres db (TableEntity table) -> DBT 'WithinTx m keyInner
getNextPrimaryKey tbl = do
    res <- runSelect . select $
        aggregate_ (Beam.max_ . unpackPk . pk_) (all_ tbl)
    return $ case res of
        []        -> 0
        [Nothing] -> error "Unexpected Nothing"
        [Just x]  -> x + 1
        _ : _ : _ -> error "Too many rows"

-- | Apply @cast@ SQL function.
unsafeCast_
    :: forall b a ctx syntax s.
       ( HasDefaultSqlDataType (Beam.Sql92ExpressionCastTargetSyntax syntax) b
       , Beam.IsSql92ExpressionSyntax syntax
       )
    => QGenExpr ctx syntax s a -> QGenExpr ctx syntax s b
unsafeCast_ (QExpr expr) = QExpr $ \tblPrefix ->
    Beam.castE (expr tblPrefix) (defaultSqlDataType (Proxy @b) True)

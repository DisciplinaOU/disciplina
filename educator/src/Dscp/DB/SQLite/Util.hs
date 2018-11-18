{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Utilities for writing SQLite queries.

module Dscp.DB.SQLite.Util
     ( module BeamReexport
     , checkExists
     , insertValue
     , insertExpression
     , packPk
     , valPk_
     , selectByPk
     , existsWithPk
     , currentTimestampUtc_
     ) where

import Data.Coerce (coerce)
import Data.Time.Clock (UTCTime)
import qualified Database.Beam.Backend.SQL as Beam
import Database.Beam.Query ((==.))
import Database.Beam.Query as BeamReexport (all_, asc_, default_, desc_, filter_, guard_, insert,
                                            insertValues, limit_, orderBy_, related_, select,
                                            update, val_, (/=.), (<-.), (==.), (>.), (>=.))
import qualified Database.Beam.Query as Beam
import qualified Database.Beam.Query.Internal as Beam
import Database.Beam.Schema (PrimaryKey, TableEntity)
import qualified Database.Beam.Schema as Beam
import qualified Database.Beam.Sqlite.Syntax as Beam
import qualified GHC.Generics as G

import Dscp.DB.SQLite.Functions
import Dscp.Util

-- | Check whether the query returns any row.
checkExists
    :: (MonadIO m
       ,Beam.ProjectibleWithPredicate Beam.AnyType Beam.SqliteExpressionSyntax r)
    => Beam.Q Beam.SqliteSelectSyntax db (Beam.QNested _) r -> DBT t w m Bool
checkExists query =
    fmap ((> 0) . oneOrError) $
    runSelect . Beam.select $
    Beam.aggregate_ (\_ -> Beam.countAll_) query

-- | Build a 'SqlInsertValues' from concrete table value.
insertValue :: _ => table Identity -> Beam.SqlInsertValues syntax (table (Beam.QExpr _ s))
insertValue = Beam.insertValues . one

-- | Build a 'SqlInsertValues' from concrete table value.
insertExpression
    :: _
    => (forall s'. table (Beam.QExpr _ s'))
    -> Beam.SqlInsertValues syntax (table (Beam.QExpr _ s))
insertExpression expr = Beam.insertExpressions (one expr)

#define PrimaryKeyWrapper(pk, inner) \
    ( Generic (pk) \
    , (pk) ~ PrimaryKey row Identity \
    , G.D1 d' (G.C1 c' (G.S1 s' (G.Rec0 (inner)))) ~ G.Rep (pk)) \

-- | Lift an entity to primary key, effectively just wrappes it into 'PrimaryKey' constructor.
-- Works only for primary keys which consists of one item.
packPk
    :: PrimaryKeyWrapper(pk, inner)
    => inner -> pk
packPk = G.to . coerce

-- | Wrap an entiry into a primary key sql value.
valPk_
    :: (Beam.SqlValable pk, PrimaryKeyWrapper (Beam.HaskellLiteralForQExpr pk, inner))
    => inner -> pk
valPk_ = Beam.val_ . packPk

-- | Quick way to fetch a single entiry refered by the given primary key.
selectByPk
    :: (HasCallStack, _)
    => (row Identity -> res)
    -> Beam.DatabaseEntity be db (TableEntity table)
    -> pk
    -> DBT t w m (Maybe res)
selectByPk mapper tbl key =
    fmap fetchOne $
    runSelect . Beam.select $
    Beam.filter_ (\row -> Beam.pk row ==. valPk_ key) $
    Beam.all_ tbl
  where
    fetchOne [] = Nothing
    fetchOne l  = Just (mapper $ oneOrError l)

-- | Quick way to check whether an entiry refered by the given primary key exists.
existsWithPk
    :: _
    => Beam.DatabaseEntity be db (TableEntity table) -> pk -> DBT t w m Bool
existsWithPk tbl key =
    checkExists $ Beam.filter_ (\row -> Beam.pk row ==. valPk_ key)
                               (Beam.all_ tbl)

-- | SQL CURRENT_TIMESTAMP function.
-- TODO: check it really works (returns UTC time rather than local).
-- If it does not, follow implementation of "currentTimestampE" here
-- http://hackage.haskell.org/package/beam-sqlite-0.3.2.3/docs/src/Database.Beam.Sqlite.Syntax.html#line-780
currentTimestampUtc_ :: Beam.IsSql92ExpressionSyntax syntax => Beam.QGenExpr ctxt syntax s UTCTime
currentTimestampUtc_ = Beam.QExpr (pure Beam.currentTimestampE)

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Utilities for writing SQLite queries.

module Dscp.DB.SQLite.Util
     ( module BeamReexport
     , Beam.MonadBeam

     , RelationType (..)
     , RelationT (..)
     , Relation
     , (<:-:>)
     , link_
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
     , currentTimestampUtc_
     , filterMatches_
     , filterMatchesPk_
     , getNextPrimaryKey
     ) where

import Prelude hiding (_1, _2)

import Control.Lens (Field1 (..), Field2 (..))
import Data.Coerce (coerce)
import Data.Time.Clock (UTCTime)
import qualified Database.Beam.Backend.SQL as Beam
import Database.Beam.Query as BeamReexport (QGenExpr (..), aggregate_, all_, as_, asc_, countAll_,
                                            default_, delete, desc_, exists_, filter_, guard_,
                                            insert, insertValues, leftJoin_, limit_, max_, orderBy_,
                                            references_, related_, select, update, val_, (&&.),
                                            (/=.), (<-.), (==.), (>.), (>=.), (||.))
import qualified Database.Beam.Query as Beam
import qualified Database.Beam.Query.Internal as Beam
import Database.Beam.Schema as BeamReexport (DatabaseSettings)
import qualified Database.Beam.Schema as Beam
import Database.Beam.Schema.Tables (DatabaseEntity, PrimaryKey, TableEntity)
import qualified GHC.Generics as G
import GHC.TypeLits (ErrorMessage (Text), TypeError)

import Dscp.DB.SQLite.Functions
import Dscp.Util

-- | Type of two entities relation.
data RelationType
    = Mx1  -- ^ Many-to-one
    | MxM  -- ^ Many-to-many

-- | Table which stores just a relation between two tables.
data RelationT (t :: RelationType) a b f = PrimaryKey a f :-: PrimaryKey b f
    deriving (Generic)
infix 9 :-:
{-
Somehow Beam seem not to provide this functionality ^ for purpose. Quoting the manual:

> This is the extent of beam's support for defining models. Although similar packages in other languages provide support for declaring one-to-many, many-to-one, and many-to-many relationships, beam's focused is providing a direct mapping of relational database concepts to Haskell, not on abstracting away the complexities of database querying. Thus, beam does not use 'lazy-loading' or other tricks that obfuscate performance. Because of this, the bulk of the functionality dealing with different types of relations is found in the querying support, rather than in the model declarations.

Instead, beam provides 'oneToMany' and 'manyToMany' functions which work in queries.

But having a generalized 'Relation' schema _is_ convenient, isn't it?

-}

type Relation t a b = RelationT t a b Identity

instance (rel ~ RelationT t a b f, ca ~ PrimaryKey a f) => Field1 rel rel ca ca where
    _1 f (a :-: b) = (:-: b) <$> f a
instance (rel ~ RelationT t a b f, cb ~ PrimaryKey b f) => Field2 rel rel cb cb where
    _2 f (a :-: b) = (a :-:) <$> f b

-- | Make a relation from raw ids.
(<:-:>)
    :: (PrimaryKeyWrapper (PrimaryKey a Identity) ia
       ,PrimaryKeyWrapper (PrimaryKey b Identity) ib)
    => ia -> ib -> RelationT t a b Identity
a <:-:> b = packPk a :-: packPk b
infix 9 <:-:>

-- | Require given entities to be elements of the given relation.
link_
    :: (table ~ RelationT t a b, _)
    => DatabaseEntity be db (TableEntity table)
    -> table (QGenExpr _ _ _)
    -> Beam.Q _ db _ ()
link_ relation (pk1 :-: pk2) = do
    id1 :-: id2 <- all_ relation
    guard_ (id1 ==. pk1)
    guard_ (id2 ==. pk2)

-- | Check whether the query returns any row.
checkExists
    :: (Beam.MonadBeam cmd be hdl m, _)
    => Beam.Q (Beam.Sql92SelectSyntax cmd) db (Beam.QNested _) () -> m Bool
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
    -> m (Maybe res)
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
    => Beam.DatabaseEntity be db (TableEntity table) -> pk -> m Bool
existsWithPk tbl key =
    checkExists $ do
        row <- all_ tbl
        guard_ (Beam.pk row ==. valPk_ key)

-- | Quick way to delete a single entiry refered by the given primary key.
deleteByPk
    :: (Beam.MonadBeam cmd be hdl m, Beam.IsSql92Syntax cmd, Beam.Table table,
        PrimaryKeyWrapper (PrimaryKey table Identity) pk, _
       )
    => Beam.DatabaseEntity be db (TableEntity table)
    -> pk
    -> m ()
deleteByPk tbl key = runDelete $ delete tbl (valPk_ key `references_`)

-- | SQL CURRENT_TIMESTAMP function.
currentTimestampUtc_
    :: forall ctxt syntax s.
       Beam.IsSql92ExpressionSyntax syntax
    => Beam.QGenExpr ctxt syntax s UTCTime
currentTimestampUtc_ =
    -- The behavior should not depend much on the backend.
    -- CURRENT_TIMESTAMP is nicely fetched both for "datetime" and "timestamp" types.
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
getNextPrimaryKey
    :: ( PrimaryKeyWrapper (PrimaryKey table Identity) keyInner
       , Num keyInner
       , Beam.MonadBeam cmd be hdl m
       , Beam.Table table
       , Beam.Database be db
       , Beam.FromBackendRow be keyInner
       -- , Coercible keyInner Int
       , _
       )
    => Beam.DatabaseEntity be db (TableEntity table) -> m keyInner
getNextPrimaryKey tbl = do
    res <- runSelect . select $
        aggregate_ (Beam.max_ . unpackPk . pk_) (all_ tbl)
    return $ case res of
        []        -> 0
        [Nothing] -> error "Unexpected Nothing"
        [Just x]  -> x + 1
        _ : _ : _ -> error "Too many rows"

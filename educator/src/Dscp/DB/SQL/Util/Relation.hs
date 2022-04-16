{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Simple relations creation.

module Dscp.DB.SQL.Util.Relation
     ( RelationType (..)
     , RelationT (..)
     , Relation
     , (<:-:>)
     , link_
     ) where

import Universum hiding (_1, _2)

import Control.Lens (Field1 (..), Field2 (..))
import Database.Beam.Query (Q)
import Database.Beam.Schema (Beamable, DatabaseEntity, PrimaryKey, Table (..), TableEntity)

import Dscp.DB.SQL.Util.Common

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

instance (Typeable a, Typeable b, Beamable (PrimaryKey a), Beamable (PrimaryKey b)) =>
         Table (RelationT 'Mx1 a b) where
    newtype PrimaryKey (RelationT 'Mx1 a b) f = Mx1RelationRowId (PrimaryKey a f)
        deriving (Generic)
    primaryKey (a :-: _) = Mx1RelationRowId a

instance (Typeable a, Typeable b, Beamable (PrimaryKey a), Beamable (PrimaryKey b)) =>
         Table (RelationT 'MxM a b) where
    data PrimaryKey (RelationT 'MxM a b) f = MxMRelationRowId (PrimaryKey a f) (PrimaryKey b f)
        deriving (Generic)
    primaryKey (a :-: b) = MxMRelationRowId a b

instance (Beamable (PrimaryKey a), Beamable (PrimaryKey b)) =>
         Beamable (RelationT t a b)

instance (Beamable (PrimaryKey a)) =>
         Beamable (PrimaryKey $ RelationT 'Mx1 a b)

instance (Beamable (PrimaryKey a), Beamable (PrimaryKey b)) =>
         Beamable (PrimaryKey $ RelationT 'MxM a b)

-- | Make a relation from raw ids.
(<:-:>)
    :: ( PrimaryKeyWrapper (PrimaryKey a Identity) ia
       , PrimaryKeyWrapper (PrimaryKey b Identity) ib
       )
    => ia -> ib -> RelationT t a b Identity
a <:-:> b = packPk a :-: packPk b
infix 9 <:-:>

-- | Require given entities to be elements of the given relation.
link_
    :: (table ~ RelationT t a b, _)
    => DatabaseEntity be db (TableEntity table)
    -> table (QGenExpr _ _ _)
    -> Q be db _ ()
link_ relation (pk1 :-: pk2) = do
    id1 :-: id2 <- all_ relation
    guard_ (id1 ==. pk1)
    guard_ (id2 ==. pk2)

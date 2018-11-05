
{-# language ExistentialQuantification #-}

module Dscp.DB.CanProvideDB.Class
    ( Ids
    , Values
    , Prefix

    , Plugin(..)
    , Operation(..)
    , CanUseDb
    , ProvidesPlugin(..)

    , get
    , batch
    )
    where

import Prelude hiding (get)

import Codec.Serialise
import UnliftIO (MonadUnliftIO)

import Dscp.Snowdrop.Configuration (Ids, Values)

type Prefix = Int

type CanUseDb k v m = (Serialise k,   Serialise v,      MonadIO m, MonadThrow m, MonadUnliftIO m)
type SupportsDb   m = (Serialise Ids, Serialise Values, MonadIO m, MonadThrow m, MonadUnliftIO m)

data Operation k v
    = Put k v
    | Delete k

data Plugin = Plugin
    { pGet           :: forall k v m. CanUseDb k v m =>  k              -> m (Maybe v)
    , pBatch         :: forall k v m. CanUseDb k v m => [Operation k v] -> m ()

    , pGetWithPrefix :: forall m.   SupportsDb m => Prefix -> Ids -> m (Maybe Values)
    , pDoWithPrefix  :: forall m.   SupportsDb m => Prefix -> Operation Ids Values -> m ()
    , pIterate       :: forall m b. SupportsDb m => Prefix -> b -> (b -> (Ids, Values) -> b) -> m b
    }

class ProvidesPlugin m where
    providePlugin :: m Plugin

get :: CanUseDb k v m => k -> ReaderT Plugin m (Maybe v)
get k = do
    method <- asks pGet
    lift $ method k

batch :: forall k v m . CanUseDb k v m => [Operation k v] -> ReaderT Plugin m ()
batch block = do
    method <- asks pBatch
    lift $ method block

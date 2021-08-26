
module Dscp.DB.CanProvideDB.Rocks (RocksDB, plugin) where

import Codec.Serialise

import Dscp.DB.CanProvideDB.Class as Abstract
import Dscp.DB.CanProvideDB.Common
import Dscp.DB.Rocks as Rocks
import Dscp.Util.Serialise (serialise')

plugin :: RocksDB -> Plugin
plugin (RocksDB rdb) = Plugin
    { pGet   = Rocks.get rdb
    , pBatch = Rocks.writeBatch rdb . map toRocksBatch

    , pGetWithPrefix = Rocks.getWith rdb . idToKey

    , pDoWithPrefix  = \prefix op -> do
        Rocks.writeBatch rdb [toRocksBatch' prefix op]

    , pIterate = \prefix start folder ->
        Rocks.iterate rdb (prefixName prefix) start $ \acc (k, v) -> do
            folder acc
                ( deserialiseErr ("iter key for prefix "   <> show prefix) k
                , deserialiseErr ("iter value for prefix " <> show prefix) v
                )
    }

toRocksBatch :: (Serialise k, Serialise v) => Operation k v -> Rocks.BatchOp
toRocksBatch op = case op of
    Abstract.Put    k v -> Rocks.Put (serialise' k) (serialise' v)
    Abstract.Delete k   -> Rocks.Del (serialise' k)

toRocksBatch' :: (Serialise k, Serialise v) => Prefix -> Operation k v -> Rocks.BatchOp
toRocksBatch' i op = case op of
    Abstract.Put    k v -> Rocks.Put (idToKey i k) (serialise' v)
    Abstract.Delete k   -> Rocks.Del (idToKey i k)

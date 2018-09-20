-- | "AllocResource" instances for rocksdb.

module Dscp.Resource.Rocks () where

import Dscp.DB.Rocks.Real (RocksDB, RocksDBParams, closeNodeDB, openNodeDB)
import Dscp.Resource.Class (AllocResource (..), buildComponentR)

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance AllocResource RocksDB where
    type Deps RocksDB = RocksDBParams
    allocResource p = buildComponentR "RocksDB" (openNodeDB p) closeNodeDB

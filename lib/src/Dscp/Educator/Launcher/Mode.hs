{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of Educator's WorkMode and its implementations.

module Dscp.Educator.Launcher.Mode
    (
      -- * Constraints
      EducatorWorkMode
    , CombinedWorkMode

      -- * Implementations
    , EducatorContext (..)
    , EducatorRealMode
    , ecWitnessCtx
    , ecDB
    ) where

import Control.Lens (makeLenses)
import Loot.Base.HasLens (HasLens (..), HasLens')
import Loot.Log.Rio (LoggingIO)
import Loot.Network.ZMQ as Z

import Dscp.DB.Rocks.Real.Types (RocksDB)
import Dscp.DB.SQLite (MonadSQLiteDB, SQLiteDB)
import Dscp.Educator.Launcher.Params (EducatorParams)
import Dscp.Educator.Secret (MonadEducatorSecret)
import Dscp.Educator.Secret.Types (EducatorSecret)
import qualified Dscp.Launcher.Mode as Basic
import Dscp.Launcher.Rio (RIO)
import qualified Dscp.Witness as W
import Dscp.Witness.Mempool (MempoolVar)

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of bare Educator node.
type EducatorWorkMode ctx m =
    ( Basic.BasicWorkMode m
    , MonadSQLiteDB m
    , MonadEducatorSecret m

    , MonadReader ctx m

    , HasLens' ctx EducatorParams
    , HasLens' ctx SQLiteDB
    , HasLens' ctx EducatorSecret
    )

-- | Set of typeclasses which define capabilities both of Educator and W.
type CombinedWorkMode ctx m =
    ( EducatorWorkMode ctx m
    , W.WitnessWorkMode ctx m
    )

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

-- TODO add parameters
-- TODO Separate resources and non-resources.
data EducatorContext = EducatorContext
    {
      _ecParams     :: EducatorParams
    , _ecDB         :: SQLiteDB
    , _ecSecret     :: EducatorSecret

    , _ecWitnessCtx :: W.WitnessContext
    }

makeLenses ''EducatorContext

type EducatorRealMode = RIO EducatorContext

---------------------------------------------------------------------
-- HasLens
---------------------------------------------------------------------

instance HasLens EducatorParams EducatorContext EducatorParams where
    lensOf = ecParams
instance HasLens SQLiteDB EducatorContext SQLiteDB where
    lensOf = ecDB
instance HasLens EducatorSecret EducatorContext EducatorSecret where
    lensOf = ecSecret

instance HasLens W.WitnessParams EducatorContext W.WitnessParams where
    lensOf = ecWitnessCtx . lensOf @W.WitnessParams
instance HasLens LoggingIO EducatorContext LoggingIO where
    lensOf = ecWitnessCtx . lensOf @LoggingIO
instance HasLens RocksDB EducatorContext RocksDB where
    lensOf = ecWitnessCtx . lensOf @RocksDB
instance HasLens Z.ZTGlobalEnv EducatorContext Z.ZTGlobalEnv where
    lensOf = ecWitnessCtx . lensOf @Z.ZTGlobalEnv
instance HasLens Z.ZTNetCliEnv EducatorContext Z.ZTNetCliEnv where
    lensOf = ecWitnessCtx . lensOf @Z.ZTNetCliEnv
instance HasLens Z.ZTNetServEnv EducatorContext Z.ZTNetServEnv where
    lensOf = ecWitnessCtx . lensOf @Z.ZTNetServEnv
instance HasLens MempoolVar EducatorContext MempoolVar where
    lensOf = ecWitnessCtx . lensOf @MempoolVar
instance HasLens W.SDActions EducatorContext W.SDActions where
    lensOf = ecWitnessCtx . lensOf @W.SDActions

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

_sanity :: EducatorRealMode ()
_sanity = _sanityCallee
  where
    _sanityCallee :: CombinedWorkMode ctx m => m ()
    _sanityCallee = pass

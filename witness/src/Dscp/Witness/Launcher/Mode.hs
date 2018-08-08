{-# LANGUAGE TemplateHaskell #-}

-- | Module contains the definition of Witness's WorkMode and its implementations.

module Dscp.Witness.Launcher.Mode
    (
      -- * Markers
      WitnessNode

      -- * Constraints
    , WitnessWorkMode

      -- * Implementations
    , WitnessContext (..)
    , wcResources

      -- * RealMode
    , WitnessRealMode

      -- * Todo: move to place they belong
    , TxRelayInput (..), newTxRelayInput
    , TxRelayPipe (..),  newTxRelayPipe
    , relayTx
    ) where

import qualified Control.Concurrent.STM as STM
import Control.Lens (makeLenses)

import Loot.Base.HasLens (HasLens (..), HasLens')
import Loot.Log.Rio (LoggingIO)
import Loot.Network.Class (NetworkingCli, NetworkingServ)
import Loot.Network.ZMQ as Z

import Dscp.Core.Foundation.Witness (GTxWitnessed)
import Dscp.DB.Rocks.Class (MonadDB)
import Dscp.DB.Rocks.Real.Types (RocksDB)
import qualified Dscp.Launcher.Mode as Basic
import Dscp.Launcher.Rio (RIO)
import Dscp.Network ()
import Dscp.Resource.Keys (KeyResources)
import Dscp.Snowdrop.Actions (SDActions)
import Dscp.Witness.Config (HasWitnessConfig, withWitnessConfig)
import Dscp.Witness.Launcher.Marker (WitnessNode)
import Dscp.Witness.Launcher.Params (WitnessParams)
import Dscp.Witness.Launcher.Resource (WitnessResources, wrDB, wrKey, wrLogging, wrNetwork)
import Dscp.Witness.Mempool (MempoolVar)

---------------------------------------------------------------------
-- WorkMode class
---------------------------------------------------------------------

-- | Set of typeclasses which define capabilities of Witness node.
type WitnessWorkMode ctx m =
    ( Basic.BasicWorkMode m
    , MonadDB m
    , NetworkingCli ZmqTcp m
    , NetworkingServ ZmqTcp m

    , HasWitnessConfig

    , MonadReader ctx m

    , HasLens' ctx WitnessParams
    , HasLens' ctx LoggingIO
    , HasLens' ctx RocksDB
    , HasLens' ctx Z.ZTGlobalEnv
    , HasLens' ctx Z.ZTNetCliEnv
    , HasLens' ctx Z.ZTNetServEnv
    , HasLens' ctx MempoolVar
    , HasLens' ctx SDActions
    , HasLens' ctx (KeyResources WitnessNode)
    , HasLens' ctx TxRelayInput
    , HasLens' ctx TxRelayPipe
    )

---------------------------------------------------------------------
-- WorkMode implementation
---------------------------------------------------------------------

newtype TxRelayInput = TxRelayInput { getTxRelayInput :: STM.TQueue GTxWitnessed }
newtype TxRelayPipe  = TxRelayPipe  { getTxRelayPipe  :: STM.TQueue GTxWitnessed }

relayTx :: WitnessWorkMode ctx m => GTxWitnessed -> m ()
relayTx tx = do
    TxRelayInput input <- view (lensOf @TxRelayInput)
    atomically $ STM.writeTQueue input tx

newTxRelayInput :: MonadIO m => m TxRelayInput
newTxRelayInput = TxRelayInput <$> atomically STM.newTQueue

newTxRelayPipe :: MonadIO m => m TxRelayPipe
newTxRelayPipe = TxRelayPipe <$> atomically STM.newTQueue

-- | Context is resources plus some runtime variables.
data WitnessContext = WitnessContext
    { _wcParams       :: !WitnessParams     -- ^ Parameters witness was started with.
    , _wcResources    :: !WitnessResources  -- ^ Resources, allocated from params.
    , _wcMempool      :: !MempoolVar
    , _wcSDActions    :: !SDActions
    , _wcTxRelayInput :: !TxRelayInput
    , _wcTxRelayPipe  :: !TxRelayPipe
    }


makeLenses ''WitnessContext

type WitnessRealMode = RIO WitnessContext

---------------------------------------------------------------------
-- HasLens
---------------------------------------------------------------------

instance HasLens WitnessParams WitnessContext WitnessParams where
    lensOf = wcParams
instance HasLens LoggingIO WitnessContext LoggingIO where
    lensOf = wcResources . wrLogging
instance HasLens RocksDB WitnessContext RocksDB where
    lensOf = wcResources . wrDB
instance HasLens Z.ZTGlobalEnv WitnessContext Z.ZTGlobalEnv where
    lensOf = wcResources . wrNetwork . lensOf @Z.ZTGlobalEnv
instance HasLens Z.ZTNetCliEnv WitnessContext Z.ZTNetCliEnv where
    lensOf = wcResources . wrNetwork . lensOf @Z.ZTNetCliEnv
instance HasLens Z.ZTNetServEnv WitnessContext Z.ZTNetServEnv where
    lensOf = wcResources . wrNetwork . lensOf @Z.ZTNetServEnv
instance HasLens (KeyResources WitnessNode) WitnessContext (KeyResources WitnessNode) where
    lensOf = wcResources . wrKey
instance HasLens MempoolVar WitnessContext MempoolVar where
    lensOf = wcMempool
instance HasLens SDActions WitnessContext SDActions where
    lensOf = wcSDActions
instance HasLens TxRelayInput WitnessContext TxRelayInput where
    lensOf = wcTxRelayInput
instance HasLens TxRelayPipe WitnessContext TxRelayPipe where
    lensOf = wcTxRelayPipe

----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

_sanity :: WitnessRealMode ()
_sanity = withWitnessConfig (error "") _sanityCallee
  where
    _sanityCallee :: WitnessWorkMode ctx m => m ()
    _sanityCallee = pass

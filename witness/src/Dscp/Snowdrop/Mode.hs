-- | SD execution parameters.

module Dscp.Snowdrop.Mode
    ( IOCtx
    , SdM_
    , SdM
    , StateSdM
    , runSdRIO
    , runSdM
    , runSdMRead
    , runStateSdM
    , runStateSdMRead
    ) where

import Data.Default (def)
import Loot.Base.HasLens (lensOf)
import Loot.Log.Rio (LoggingIO)
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Execution as SD
import Snowdrop.Util (RIO, runRIO)

import Dscp.Snowdrop.Actions
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.IOCtx (IOCtx)
import Snowdrop.Execution (AVLChgAccum, RememberForProof)
import Dscp.Witness.Launcher.Mode
import Dscp.Witness.AVL (AvlHash)
import qualified Dscp.Witness.SDLock as Lock

-- | Alias for ERoComp with concrete config types.
type SdM_ chgacc = SD.ERoComp Exceptions Ids Values (IOCtx chgacc)

-- | Monad representing actions in snowdrop BaseM, related to rocksdb storage.
type SdM a = SdM_ (SD.SumChangeSet Ids Values) a

-- | Monad representing actions in snowdrop BaseM, related to AVL+ storage.
type StateSdM a = SdM_ (AVLChgAccum AvlHash Ids Values) a

-- This is terrible
runSdRIO :: WitnessWorkMode ctx m => RIO LoggingIO a -> m a
runSdRIO action = do
    logger <- view (lensOf @LoggingIO)
    liftIO $ runRIO logger action

-- | SdM runner, should be protected by some lock.
runSdM :: (WitnessWorkMode ctx m, Lock.WithinReadSDLock) => SdM a -> m a
runSdM action = do
    blockDBA <- SD.dmaAccessActions . nsBlockDBActions <$> view (lensOf @SDActions)
    SD.runERoCompIO @Exceptions blockDBA def action

-- | Often used SdM runner that takes read lock.
runSdMRead :: WitnessWorkMode ctx m => SdM a -> m a
runSdMRead action = Lock.readingSDLock $ runSdM action

-- | SdM runner, should be protected by some lock.
runStateSdM
    :: (WitnessWorkMode ctx m, Lock.WithinReadSDLock)
    => RememberForProof -> StateSdM a -> m a
runStateSdM recForProof action = do
    stateDBA <- SD.dmaAccessActions . flip nsStateDBActions recForProof <$> view (lensOf @SDActions)
    SD.runERoCompIO @Exceptions stateDBA def action

-- | Often used SdM runner that takes read lock.
runStateSdMRead :: WitnessWorkMode ctx m => RememberForProof -> StateSdM a -> m a
runStateSdMRead recForProof action = Lock.readingSDLock $ runStateSdM recForProof action

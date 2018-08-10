-- | SD execution parameters.

module Dscp.Snowdrop.Mode
    ( IOCtx
    , SdM_
    , SdM
    , StateSdM
    , runSdRIO
    , runSdMRead
    , runSdMWrite
    , runStateSdMRead
    , runStateSdMWrite
    ) where

import Data.Default (def)
import Loot.Base.HasLens (lensOf)
import Loot.Log.Rio (LoggingIO)
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Model.Execution as SD
import Snowdrop.Util (RIO, runRIO)

import Dscp.Snowdrop.Actions
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.IOCtx (IOCtx)
import Dscp.Snowdrop.Storage.Avlp
import Dscp.Witness.Launcher.Mode
import qualified Dscp.Witness.SDLock as Lock

-- | Alias for ERoComp with concrete config types.
type SdM_ chgacc = SD.ERoComp Exceptions Ids Values (IOCtx chgacc)

-- | Monad representing actions in snowdrop BaseM, related to rocksdb storage.
type SdM a = SdM_ (SD.SumChangeSet Ids Values) a

-- | Monad representing actions in snowdrop BaseM, related to AVL+ storage.
type StateSdM a = SdM_ (AVLChgAccum Ids Values) a

-- This is terrible
runSdRIO :: WitnessWorkMode ctx m => RIO LoggingIO a -> m a
runSdRIO action = do
    logger <- view (lensOf @LoggingIO)
    liftIO $ runRIO logger action

properlyRunERoComp :: WitnessWorkMode ctx m => SdM a -> m a
properlyRunERoComp action = do
    blockDBA <- SD.dmaAccessActions . nsBlockDBActions <$> view (lensOf @SDActions)
    SD.runERoCompIO @Exceptions blockDBA def action

-- | SdM runner that takes read lock.
runSdMRead :: WitnessWorkMode ctx m => SdM a -> m a
runSdMRead = Lock.readingSDLock . properlyRunERoComp

-- | SdM runner that takes exclusive lock.
runSdMWrite :: WitnessWorkMode ctx m => SdM a -> m a
runSdMWrite = Lock.writingSDLock . properlyRunERoComp

properlyRunStateERoComp :: WitnessWorkMode ctx m => RememberForProof -> StateSdM a -> m a
properlyRunStateERoComp recForProof action = do
    stateDBA <- SD.dmaAccessActions . flip nsStateDBActions recForProof <$> view (lensOf @SDActions)
    SD.runERoCompIO @Exceptions stateDBA def action

-- | SdM runner that takes read lock.
runStateSdMRead :: WitnessWorkMode ctx m => RememberForProof -> StateSdM a -> m a
runStateSdMRead recForProof = Lock.readingSDLock . properlyRunStateERoComp recForProof

-- | SdM runner that takes exclusive lock.
runStateSdMWrite :: WitnessWorkMode ctx m => RememberForProof -> StateSdM a -> m a
runStateSdMWrite recForProof = Lock.writingSDLock . properlyRunStateERoComp recForProof

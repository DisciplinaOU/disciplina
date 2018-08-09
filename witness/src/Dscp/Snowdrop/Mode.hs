-- | SD execution parameters.

module Dscp.Snowdrop.Mode
    ( IOCtx
    , SdM_
    , SdM
    , runSdMRead
    , runSdMWrite
    ) where

import Data.Default (def)
import Loot.Base.HasLens (lensOf)
import qualified Snowdrop.Model.Execution as SD
import qualified Snowdrop.Model.State.Core as SD

import Dscp.Snowdrop.Actions
import Dscp.Snowdrop.Configuration
import Dscp.Witness.Launcher.Mode
import qualified Dscp.Witness.SDLock as Lock


type IOCtx chgAccum = SD.IOCtx chgAccum Ids Values

-- | Alias for ERoComp with concrete config types.
type SdM_ chgacc = SD.ERoComp Exceptions Ids Values (IOCtx chgacc)

-- | Monad representing actions in snowdrop BaseM, related to rocksdb storage.
type SdM a = SdM_ (SD.SumChangeSet Ids Values) a

-- | SdM runner.
runSdMRead :: WitnessWorkMode ctx m => SdM a -> m a
runSdMRead action = do
    blockDBA <- SD.dmaAccessActions . nsBlockDBActions <$> view (lensOf @SDActions)
    Lock.reading $ do
        liftIO $ SD.runERoCompIO @Exceptions blockDBA def $ action

-- | SdM runner.
runSdMWrite :: WitnessWorkMode ctx m => SdM a -> m a
runSdMWrite action = do
    blockDBA <- SD.dmaAccessActions . nsBlockDBActions <$> view (lensOf @SDActions)
    Lock.writing $ do
        liftIO $ SD.runERoCompIO @Exceptions blockDBA def $ action

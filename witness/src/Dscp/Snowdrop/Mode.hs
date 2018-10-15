-- | SD execution parameters.

module Dscp.Snowdrop.Mode
    ( IOCtx
    , SdM_
    , SdM
    , StateSdM
    , MonadSD
    , runSdRIO
    , runSdM
    , runSdMRead
    , runStateSdM
    , runStateSdMRead
    , unwrapSDBaseRethrow
    ) where

import Data.Default (def)
import Loot.Base.HasLens (HasCtx, lensOf)
import Loot.Log (Logging, ModifyLogName, MonadLogging)
import Loot.Log.Rio (LoggingIO)
import qualified Snowdrop.Core as SD
import Snowdrop.Execution (BaseMException (..))
import qualified Snowdrop.Execution as SD
import Snowdrop.Util (RIO, runRIO)
import UnliftIO (MonadUnliftIO)

import Dscp.Snowdrop.Actions
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.IOCtx (IOCtx)
import Dscp.Util
import Dscp.Witness.AVL (AvlHash)
import Dscp.Witness.Config
import qualified Dscp.Witness.SDLock as Lock
import Snowdrop.Execution (AVLChgAccum, RememberForProof)

-- | Alias for ERoComp with concrete config types.
type SdM_ chgacc = SD.ERoComp Exceptions Ids Values (IOCtx chgacc)

-- | Monad representing actions in snowdrop BaseM, related to rocksdb storage.
type SdM = SdM_ (SD.SumChangeSet Ids Values)

-- | Monad representing actions in snowdrop BaseM, related to AVL+ storage.
type StateSdM a = SdM_ (AVLChgAccum AvlHash Ids Values) a

type MonadSD ctx m =
    ( MonadUnliftIO m
    , MonadMask m
    , MonadLogging m
    , ModifyLogName m
    , HasCtx ctx m '[Lock.SDLock, Logging IO, SDVars]
    , HasWitnessConfig
    )

-- This is terrible
runSdRIO :: MonadSD ctx m => RIO LoggingIO a -> m a
runSdRIO action = do
    logger <- view (lensOf @LoggingIO)
    liftIO $ runRIO logger action

-- | Snowdrop's @run..IO@ throw exceptions being wrapped into 'BaseMException',
-- this functions unwraps it.
unwrapSDBaseRethrow :: MonadCatch m => m a -> m a
unwrapSDBaseRethrow = wrapRethrow $ \(BaseMException e) -> e :: Exceptions

-- | SdM runner, should be protected by some lock.
runSdM :: (MonadSD ctx m, Lock.WithinReadSDLock) => SdM a -> m a
runSdM action = do
    blockDBA <- SD.dmaAccessActions . nsBlockDBActions <$> view (lensOf @SDVars)
    unwrapSDBaseRethrow $
        SD.runERoCompIO @Exceptions blockDBA def action

-- | Often used SdM runner that takes read lock.
runSdMRead :: MonadSD ctx m => SdM a -> m a
runSdMRead action = Lock.readingSDLock $ runSdM action

-- | SdM runner, should be protected by some lock.
runStateSdM
    :: (MonadSD ctx m, Lock.WithinReadSDLock)
    => RememberForProof -> StateSdM a -> m a
runStateSdM recForProof action = do
    stateDBA <- SD.dmaAccessActions . flip nsStateDBActions recForProof <$> view (lensOf @SDVars)
    unwrapSDBaseRethrow $
        SD.runERoCompIO @Exceptions stateDBA def action

-- | Often used SdM runner that takes read lock.
runStateSdMRead :: MonadSD ctx m => RememberForProof -> StateSdM a -> m a
runStateSdMRead recForProof action = Lock.readingSDLock $ runStateSdM recForProof action

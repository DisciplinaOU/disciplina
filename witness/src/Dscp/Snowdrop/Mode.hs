-- | SD execution parameters.

module Dscp.Snowdrop.Mode
    ( IOCtx
    , ChgAccum
    , SdM
    , MonadSD
    , runSdRIO
    , runSdM
    , runSdMLocked
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
import UnliftIO (MonadUnliftIO (..))

import Dscp.DB.CanProvideDB (ProvidesPlugin (providePlugin))
import Dscp.Snowdrop.Actions
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.IOCtx (IOCtx)
import Dscp.Snowdrop.Storage.Avlp
import Dscp.Util
import Dscp.Witness.AVL (AvlHash)
import Dscp.Witness.Config
import qualified Dscp.Witness.SDLock as Lock

-- | Alias for ERoComp with concrete config types.
type SdM_ chgacc = SD.ERoComp Exceptions Ids Values (IOCtx chgacc)

type ChgAccum =
    SD.CompositeChgAccum
        (AVLChgAccum AvlHash Ids Values)
        (SD.SumChangeSet Ids Values)
        AVLPlusBlockComposition

-- | Monad representing actions in snowdrop BaseM, related to rocksdb storage.
type SdM = SdM_ ChgAccum

type MonadSD ctx m =
    ( MonadUnliftIO m
    , MonadMask m
    , MonadLogging m
    , ModifyLogName m
    , HasCtx ctx m '[Lock.SDLock, Logging IO, SDVars]
    , HasWitnessConfig
    , ProvidesPlugin m
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
    dba <- sdActionsComposition <$> view (lensOf @SDVars)
    plugin <- providePlugin
    unwrapSDBaseRethrow $ do
        initAVLStorage @AvlHash plugin initAccounts
        SD.runERoCompIO @Exceptions dba def action

-- | Often used SdM runner that takes read lock.
runSdMLocked :: MonadSD ctx m => SdM a -> m a
runSdMLocked action = Lock.readingSDLock $ runSdM action

-- | SD execution parameters.

module Dscp.Snowdrop.Mode
    ( IOCtx
    , SdM_
    , SdM
    , StateSdM
    , MonadSD
    , runSdRIO
    , runSdM
    , runSdMLocked
    , runStateSdM
    , runStateSdMLocked
    , unwrapSDBaseRethrow

    -- * Generalized SD runners
    , SdReadMode (..)
    , SdReadM (..)
    , KnownSdReadMode (..)
    , runSdReadMLocked
    , lift2xSdM
    ) where

import Data.Coerce (coerce)
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
import Dscp.Witness.Launcher.Context
import qualified Dscp.Witness.SDLock as Lock

-- | Alias for ERoComp with concrete config types.
type SdM_ chgacc = SD.ERoComp Exceptions Ids Values (IOCtx chgacc)

type SumChangeSet = SD.SumChangeSet Ids Values

-- | Monad representing actions in snowdrop BaseM, related to rocksdb storage.
type SdM = SdM_ SumChangeSet

-- | Monad representing actions in snowdrop BaseM, related to AVL+ storage.
type StateSdM a = SdM_ (AVLChgAccum AvlHash Ids Values) a

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
    logger <- view lensOf
    liftIO $ runRIO logger action

-- | Snowdrop's @run..IO@ throw exceptions being wrapped into 'BaseMException',
-- this functions unwraps it.
unwrapSDBaseRethrow :: MonadCatch m => m a -> m a
unwrapSDBaseRethrow = wrapRethrow $ \(BaseMException e) -> e :: Exceptions

-- | SdM runner, should be protected by some lock.
runSdM :: (MonadSD ctx m, Lock.WithinReadSDLock) => SdM a -> m a
runSdM action = do
    blockDBA <- view lensOf <&> SD.dmaAccessActions . nsBlockDBActions
    plugin   <- providePlugin
    unwrapSDBaseRethrow $ do
        initAVLStorage @AvlHash plugin initAccounts

        SD.runERoCompIO @Exceptions blockDBA def action

-- | Often used SdM runner that takes read lock.
runSdMLocked :: MonadSD ctx m => SdM a -> m a
runSdMLocked action = Lock.readingSDLock $ runSdM action

-- | SdM runner, should be protected by some lock.
runStateSdM
    :: (MonadSD ctx m, Lock.WithinReadSDLock)
    => StateSdM a -> m a
runStateSdM action = do
    stateDBA <- view lensOf <&> SD.dmaAccessActions . nsStateDBActions
    plugin   <- providePlugin
    unwrapSDBaseRethrow $ do
        initAVLStorage @AvlHash plugin initAccounts

        SD.runERoCompIO @Exceptions stateDBA def action

-- | Often used SdM runner that takes read lock.
runStateSdMLocked :: MonadSD ctx m => StateSdM a -> m a
runStateSdMLocked action = Lock.readingSDLock $ runStateSdM action

----------------------------------------------------------------------------
-- Generalized SD runners
----------------------------------------------------------------------------

-- | Which version of snowdrop data should be read.
data SdReadMode = ChainOnly | ChainAndMempool

-- | Wrapper over an action to indicate read mode it should be executed in.
-- Sometimes we want an action to work both in chain-only and chain+mempool modes
-- and have an access to the context at the same time, this monad is intended for such situations.
newtype SdReadM (mode :: SdReadMode) m a = SdReadM { runSdReadM :: m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader __, MonadLogging,
              MonadThrow, MonadCatch)

instance MonadTrans (SdReadM mode) where
    lift = SdReadM

instance MonadUnliftIO m => MonadUnliftIO (SdReadM mode m) where
    askUnliftIO = lift $ coerce <$> askUnliftIO @m

runSdReadMLocked
    :: forall mode m ctx a. MonadSD ctx m
    => (Lock.WithinReadSDLock => SdReadM mode m a) -> m a
runSdReadMLocked action = Lock.readingSDLock $ runSdReadM action

-- | Lifting snowdrop 'SdM'-like actions to 'SdReadM'.
class KnownSdReadMode (mode :: SdReadMode) where
    type SdReadModeChgAcc mode :: *
    liftSdM
        :: (WitnessWorkMode ctx m, Lock.WithinReadSDLock)
        => SdM_ (SdReadModeChgAcc mode) a -> SdReadM mode m a

-- | Commonly used double lift.
lift2xSdM
    :: (WitnessWorkMode ctx m, Lock.WithinReadSDLock, KnownSdReadMode mode, MonadTrans t)
    => SdM_ (SdReadModeChgAcc mode) a -> t (SdReadM mode m) a
lift2xSdM = lift . liftSdM

instance KnownSdReadMode 'ChainOnly where
    type SdReadModeChgAcc 'ChainOnly = SumChangeSet
    liftSdM = lift . runSdM

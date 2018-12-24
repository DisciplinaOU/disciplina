-- | SD extended reading mode.

module Dscp.Snowdrop.ReadMode
    ( SdReadMode (..)
    , SdReadM (..)
    , KnownSdReadMode (..)
    , runSdReadMLocked
    , lift2xSdM
    ) where

import Data.Coerce (coerce)
import Loot.Log (MonadLogging)
import UnliftIO (MonadUnliftIO (..))

import Dscp.Snowdrop.Mode
import Dscp.Witness.Launcher.Context
import qualified Dscp.Witness.SDLock as Lock

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
    liftSdM
        :: (WitnessWorkMode ctx m, Lock.WithinReadSDLock)
        => SdM a -> SdReadM mode m a

-- | Commonly used double lift.
lift2xSdM
    :: (WitnessWorkMode ctx m, KnownSdReadMode mode, Lock.WithinReadSDLock, MonadTrans t)
    => SdM a -> t (SdReadM mode m) a
lift2xSdM = lift . liftSdM

instance KnownSdReadMode 'ChainOnly where
    liftSdM = lift . runSdM

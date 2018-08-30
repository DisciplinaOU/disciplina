
module Dscp.Witness.Relay
    ( relayTx
    , newRelayState
    , RelayState (RelayState)
    , rsInput
    , rsPipe
    , rsFailed
    ) where

import qualified Control.Concurrent.STM as STM
import Control.Lens (makeLenses)
import qualified Data.Text.Buildable
import qualified Text.Show
import qualified UnliftIO.Exception as UIO

import Loot.Base.HasLens (HasLens (..), HasLens')

import Dscp.Core
import Dscp.Crypto
import Dscp.Util.Concurrent.NotifyWait

data RelayState = RelayState
    { _rsInput  :: STM.TBQueue (GTxWitnessed, Notifier "tx in mempool")
    , _rsPipe   :: STM.TBQueue GTxWitnessed
    , _rsFailed :: TVar (HashMap (Hash GTxWitnessed) (GTxWitnessed, SomeException))
    }

makeLenses ''RelayState

data RelayException
    = TxQueueFull
    deriving (Eq, Generic, Typeable)

instance Exception RelayException

instance Buildable RelayException where
    build TxQueueFull = "Transaction queue overflow"

instance Show RelayException where
    show = toString . pretty

-- these sized do not really matter for now
relayInputSize, relayPipeSize :: Int
relayInputSize = 100
relayPipeSize = 100

newRelayState :: MonadIO m => m RelayState
newRelayState = atomically $
    pure RelayState
        <*> STM.newTBQueue relayInputSize
        <*> STM.newTBQueue relayPipeSize
        <*> STM.newTVar mempty

relayTx
    :: (MonadReader ctx m, HasLens' ctx RelayState, MonadIO m)
    => GTxWitnessed
    -> m (Waiter "tx in mempool")
relayTx tx = do
    input <- view (lensOf @RelayState . rsInput)
    (notifier, waiter) <- newWaitPair
    isFull <- atomically $ do
        full <- STM.isFullTBQueue input
        when (not full) $
            STM.writeTBQueue input (tx, notifier)
        return full
    when isFull $
        UIO.throwIO TxQueueFull
    return waiter

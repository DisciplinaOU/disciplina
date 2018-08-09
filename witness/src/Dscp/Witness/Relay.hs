
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

import Loot.Base.HasLens (HasLens (..), HasLens')

import Dscp.Core
import Dscp.Crypto

data RelayState = RelayState
    { _rsInput  :: STM.TQueue GTxWitnessed
    , _rsPipe   :: STM.TQueue GTxWitnessed
    , _rsFailed :: TVar (HashMap (Hash GTxWitnessed) GTxWitnessed)
    }

makeLenses ''RelayState

newRelayState :: MonadIO m => m RelayState
newRelayState = atomically $
    pure RelayState
        <*> STM.newTQueue
        <*> STM.newTQueue
        <*> STM.newTVar mempty

relayTx
    :: (MonadReader ctx m, HasLens' ctx RelayState, MonadIO m)
    => GTxWitnessed
    -> m ()
relayTx tx = do
    input <- view (lensOf @RelayState . rsInput)
    atomically $ STM.writeTQueue input tx

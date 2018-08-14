
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
    { _rsInput  :: STM.TBQueue GTxWitnessed
    , _rsPipe   :: STM.TBQueue GTxWitnessed
    , _rsFailed :: TVar (HashMap (Hash GTxWitnessed) GTxWitnessed)
    }

makeLenses ''RelayState

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
    -> m ()
relayTx tx = do
    input <- view (lensOf @RelayState . rsInput)
    atomically $ STM.writeTBQueue input tx

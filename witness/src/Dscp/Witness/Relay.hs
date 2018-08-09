
module Dscp.Witness.Relay
    ( relayTx
    , newRelayInput
    , newRelayPipe
    ) where

import qualified Control.Concurrent.STM as STM

import Loot.Base.HasLens (HasLens (..))

import qualified Dscp.Core as Core
import Dscp.Witness.Launcher.Mode (TxRelayInput (..), TxRelayPipe (..), WitnessWorkMode)

relayTx :: WitnessWorkMode ctx m => Core.GTxWitnessed -> m ()
relayTx gtx = do
    TxRelayInput input <- view (lensOf @TxRelayInput)
    atomically $ STM.writeTQueue input gtx

newRelayInput :: MonadIO m => m TxRelayInput
newRelayInput = TxRelayInput <$> atomically STM.newTQueue

newRelayPipe :: MonadIO m => m TxRelayPipe
newRelayPipe = TxRelayPipe <$> atomically STM.newTQueue


module Dscp.Witness.Relay
    ( tx
    , newInput
    , newPipe
    ) where

import qualified Control.Concurrent.STM as STM

import Loot.Base.HasLens (HasLens (..))

import qualified Dscp.Core as Core
import Dscp.Witness.Launcher.Mode (TxRelayInput (..), TxRelayPipe (..), WitnessWorkMode)

tx :: WitnessWorkMode ctx m => Core.GTxWitnessed -> m ()
tx gtx = do
    TxRelayInput input <- view (lensOf @TxRelayInput)
    atomically $ STM.writeTQueue input gtx

newInput :: MonadIO m => m TxRelayInput
newInput = TxRelayInput <$> atomically STM.newTQueue

newPipe :: MonadIO m => m TxRelayPipe
newPipe = TxRelayPipe <$> atomically STM.newTQueue


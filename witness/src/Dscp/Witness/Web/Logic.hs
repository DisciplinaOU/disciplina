module Dscp.Witness.Web.Logic
       ( getAccountState
       , submitUserTx
       , submitUserTxAsync
       ) where

import Control.Lens (views)
import Loot.Base.HasLens (lensOf)
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Model.Execution as SD
import UnliftIO.Async (async)

import Dscp.Core
import Dscp.Snowdrop
import Dscp.Util (assertJust)
import Dscp.Witness.Launcher.Mode (WitnessWorkMode)
import qualified Dscp.Witness.Relay as Relay
import qualified Dscp.Witness.SDLock as Lock
import Dscp.Witness.Web.Error
import Dscp.Witness.Web.Types

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

noAccount :: WitnessWebError
noAccount = EntityAbsent "No such address registered"

----------------------------------------------------------------------------
-- Logic
----------------------------------------------------------------------------

getAccount :: WitnessWorkMode ctx m => Address -> m Account
getAccount address = do
    blockActs <-
        views (lensOf @SDActions)
              (SD.dmaAccessActions . flip nsStateDBActions (RememberForProof False))
    maccount <- Lock.readingSDLock $ do
        SD.runERoCompIO @Exceptions blockActs Nothing $
            SD.queryOne (AccountId address)
    pure maccount `assertJust` noAccount

pickAccountBalance :: WitnessWorkMode ctx m => Account -> m Balances
pickAccountBalance blockAcc = do
    return Balances
        { bConfirmed = Coin . fromIntegral $ aBalance blockAcc
        }

getAccountState :: WitnessWorkMode ctx m => Address -> m AccountState
getAccountState addr = do
    account <- getAccount addr
    balances <- pickAccountBalance account
    return AccountState
        { asBalances = balances
        , asNextNonce = aNonce account + 1
        }

-- | Applies transaction everywhere.
submitUserTx :: WitnessWorkMode ctx m => TxWitnessed -> m ()
submitUserTx = Relay.relayTx . GMoneyTxWitnessed

-- | Applies transaction, but does not wait for a whole cycle of transaction
-- application.
submitUserTxAsync :: WitnessWorkMode ctx m => TxWitnessed -> m ()
submitUserTxAsync tw = void . async $ submitUserTx tw

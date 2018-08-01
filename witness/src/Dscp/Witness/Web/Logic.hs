module Dscp.Witness.Web.Logic
       ( getAccountState
       , submitUserTx
       ) where

import Control.Lens (views)
import Loot.Base.HasLens (HasLens', lensOf)
import qualified Snowdrop.Model.Execution as SD
import qualified Snowdrop.Model.State.Core as SD

import Dscp.Core
import Dscp.Core.Foundation.Transactions
import Dscp.Snowdrop
import Dscp.Util (assertJust)
import Dscp.Witness.Mempool
import Dscp.Witness.Web.Error
import Dscp.Witness.Web.Types

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

noAccount :: WitnessWebError
noAccount = EntityAbsent "No account corresponding to secret key found"

----------------------------------------------------------------------------
-- Logic
----------------------------------------------------------------------------

getAccount
    :: (MonadIO m, MonadReader ctx m, HasLens' ctx SDActions)
    => Address -> m Account
getAccount address = do
    blockActs <-
        views (lensOf @SDActions)
              (SD.dmaAccessActions . flip nsStateDBActions (RememberForProof False))
    maccount <-
        liftIO . SD.runERoCompIO @Exceptions blockActs Nothing $
        SD.queryOne (AccountId address)
    pure maccount `assertJust` noAccount

pickAccountBalance
    :: (MonadIO m, MonadReader ctx m, HasLens' ctx SDActions)
    => Account -> m Balances
pickAccountBalance blockAcc = do
    return Balances
        { bConfirmed = Coin . fromIntegral $ aBalance blockAcc
        }

getAccountState
    :: (MonadIO m, MonadReader ctx m, HasLens' ctx SDActions)
    => Address -> m AccountState
getAccountState addr = do
    account <- getAccount addr
    balances <- pickAccountBalance account
    return AccountState
        { asBalances = balances
        , asNextNonce = aNonce account + 1
        }

-- | Submit transaction.
submitUserTx
    :: (MonadIO m, MonadThrow m, MonadReader ctx m, HasLens' ctx MempoolVar)
    => TxWitnessed -> m ()
submitUserTx tw = do
    mempool <- view (lensOf @MempoolVar)
    addTxToMempool mempool (GMoneyTxWitnessed tw)

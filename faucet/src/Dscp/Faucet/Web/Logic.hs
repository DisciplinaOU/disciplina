-- | Faucet backend logic.

module Dscp.Faucet.Web.Logic
    ( faucetTransferMoneyTo
    ) where

import Control.Lens (at, views, (<<.=))
import Loot.Base.HasLens (HasCtx, lensOf)
import Serokell.Util (modifyTVarS)
import UnliftIO.MVar (withMVar)

import Dscp.Core
import Dscp.Crypto
import Dscp.Faucet.Launcher.Marker
import Dscp.Faucet.Launcher.Mode
import Dscp.Faucet.Launcher.Params
import Dscp.Faucet.Variables
import Dscp.Faucet.Web.Error
import Dscp.Resource.Keys
import Dscp.Witness.Web.Client
import Dscp.Witness.Web.Types

-- | Atomcally remember this address as being gifted or throw if it is not
-- its first time.
ensureFirstGift
    :: (MonadIO m, MonadThrow m, HasCtx ctx m '[GiftedAddresses])
    => Address -> m ()
ensureFirstGift addr = do
    giftedAddresses <- view (lensOf @GiftedAddresses)
    prevVal <- atomically . modifyTVarS giftedAddresses $
        at addr <<.= Just ()
    when (isJust prevVal) $
        throwM AddressAlreadyGifted

faucetTransferMoneyTo :: FaucetWorkMode ctx m => Address -> m ()
faucetTransferMoneyTo dest = do
    ensureFirstGift dest

    keyRes <- view (lensOf @(KeyResources FaucetApp))
    let sk = _krSecretKey keyRes
    let pk = _krPublicKey keyRes
        !source = mkAddr pk

    wc <- views (lensOf @WitnessClient) (hoistWitnessClient liftIO)
    lock <- view (lensOf @TxSendLock)
    TranslatedAmount transfer <- view (lensOf @TranslatedAmount)

    -- sad truth: we have to submit transactions sequencially in order to use
    -- sound nonces
    withMVar lock $ \() -> do
        sourceState <- wGetAccountState wc source

        -- TODO: take mempool's balance
        let balance = bConfirmed $ asBalances sourceState
        when (balance < transfer) $
            throwM SourceAccountExhausted

        let nonce = asNextNonce sourceState
            inAcc = TxInAcc{ tiaNonce = nonce, tiaAddr = source }
            outs  = one (TxOut dest transfer)
            tx    = Tx{ txInAcc = inAcc, txInValue = transfer, txOuts = outs }

            sgn = sign sk (toTxId tx, pk, ())
            witness = TxWitness{ txwSig = sgn, txwPk = pk }
            txWitnessed = TxWitnessed{ twTx = tx, twWitness = witness }

        wSubmitTxAsync wc txWitnessed

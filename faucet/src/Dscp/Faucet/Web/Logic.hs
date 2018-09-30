-- | Faucet backend logic.

module Dscp.Faucet.Web.Logic
    ( faucetTransferMoneyTo
    ) where

import Control.Lens (at, views, (<<.=))
import Loot.Base.HasLens (HasCtx, lensOf)
import Serokell.Util (modifyTVarS)
import UnliftIO.MVar (withMVar)

import Dscp.Config
import Dscp.Core
import Dscp.Crypto
import Dscp.Faucet.Config
import Dscp.Faucet.Launcher.Marker
import Dscp.Faucet.Launcher.Mode
import Dscp.Faucet.Variables
import Dscp.Faucet.Web.Error
import Dscp.Faucet.Web.Types
import Dscp.Resource.Keys
import Dscp.Util.Aeson
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

faucetTransferMoneyTo :: FaucetWorkMode ctx m => Address -> m TransferMoneyResponse
faucetTransferMoneyTo dest = do
    ensureFirstGift dest

    keyRes <- view (lensOf @(KeyResources FaucetApp))
    let sk = _krSecretKey keyRes
    let pk = _krPublicKey keyRes
        !source = mkAddr pk

    wc <- views (lensOf @WitnessClient) (hoistWitnessClient liftIO)
    lock <- view (lensOf @TxSendLock)
    let DryRun dryRun = giveL @FaucetConfig @DryRun
        TransferredAmount transfer = giveL @FaucetConfig @TransferredAmount

    -- sad truth: we have to submit transactions sequentially in order to use
    -- sound nonces
    withMVar lock $ \() -> do
        sourceState <-
            if dryRun
            then pure AccountInfo
                      { aiBalances = join BlocksOrMempool (Coin 100000)
                      , aiCurrentNonce = 6
                      , aiTransactionCount = 6
                      , aiTransactions = Nothing
                      }
            else wGetAccount wc source False

        let balance = bmTotal $ aiBalances sourceState
        when (balance < transfer) $
            throwM SourceAccountExhausted

        txWitnessed <- pure . fixFees (fcMoney $ giveL @FaucetConfig @FeeConfig) $ \fees ->
            let nonce = fromIntegral $ aiCurrentNonce sourceState
                inAcc = TxInAcc{ tiaNonce = nonce, tiaAddr = source }
                outs  = one (TxOut dest transfer)
                inVal = transfer `unsafeAddCoin` unFees fees
                tx    = Tx{ txInAcc = inAcc, txInValue = inVal, txOuts = outs }
                txId  = toTxId tx

                sgn = sign sk (txId, pk, ())
                witness = TxWitness{ txwSig = sgn, txwPk = pk }
                txWitnessed = TxWitnessed{ twTx = tx, twWitness = witness }
            in txWitnessed

        unless dryRun $
            void $ wSubmitTx wc txWitnessed

        let tx = twTx txWitnessed
            txId = toTxId tx
        return TransferMoneyResponse
            { tmrTxId = AsByteString txId
            , tmrAmount = transfer
            }

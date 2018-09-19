module Dscp.Wallet.Backend
       ( WalletFace(..)
       , createWalletFace
       ) where

import Control.Exception (throwIO)
import Control.Monad.Component (ComponentM, buildComponent_)

import Dscp.Core
import Dscp.Crypto
import Dscp.Wallet.Face
import Dscp.Wallet.KeyStorage
import Dscp.Web
import Dscp.Witness.Web.Client
import Dscp.Witness.Web.Types

createWalletFace ::
       HasCoreConfig
    => BaseUrl
    -> (WalletEvent -> IO ())
    -> ComponentM WalletFace
createWalletFace serverAddress sendEvent = buildComponent_ "Wallet" $ do
    sendStateUpdateEvent sendEvent
    wc <- createWitnessClient serverAddress
    return WalletFace
        { walletRefreshState = sendStateUpdateEvent sendEvent
        , walletGenKeyPair = genKeyPair sendEvent
        , walletRestoreKey = restoreKey sendEvent
        , walletListKeys = listKeys
        , walletSendTx = sendTx wc sendEvent
        , walletTxFee = unFees . estimateFees feeConfig . toList
        , walletGetBalance = getBalance wc
        , walletGetTxHistory = getTxHistory wc
        }

sendStateUpdateEvent :: (WalletEvent -> IO ()) -> IO ()
sendStateUpdateEvent sendEvent = getAccounts >>= sendEvent . WalletStateUpdateEvent

genKeyPair :: (WalletEvent -> IO ()) -> Maybe Text -> Maybe PassPhrase -> IO Account
genKeyPair sendEvent mName mPassPhrase = do
    (sk, pk) <- keyGen
    let account = Account
            { accountName = mName
            , accountSecretKey = encrypt (fromMaybe emptyPassPhrase mPassPhrase) sk
            , accountPublicKey = pk
            , accountAddress = mkAddr pk
            }
    addAccount account
    sendStateUpdateEvent sendEvent
    return account

restoreKey :: (WalletEvent -> IO ()) -> Maybe Text -> Encrypted SecretKey -> Maybe PassPhrase -> IO ()
restoreKey sendEvent mName eSecretKey mPassPhrase = do
    secretKey <- either throwIO return . decrypt (fromMaybe emptyPassPhrase mPassPhrase) $ eSecretKey
    let publicKey = toPublic secretKey
        account = Account
            { accountName = mName
            , accountSecretKey = eSecretKey
            , accountPublicKey = publicKey
            , accountAddress = mkAddr publicKey
            }
    addAccount account
    sendStateUpdateEvent sendEvent

listKeys :: IO [Account]
listKeys = getAccounts

sendTx ::
       HasCoreConfig
    => WitnessClient
    -> (WalletEvent -> IO ())
    -> Encrypted SecretKey
    -> Maybe PassPhrase
    -> NonEmpty TxOut
    -> IO Tx
sendTx wc sendEvent eSecretKey mPassPhrase (toList -> outs) = do
    secretKey <-
        either throwIO return .
        decrypt (fromMaybe emptyPassPhrase mPassPhrase) $ eSecretKey
    let publicKey = toPublic secretKey
        address = mkAddr publicKey

    nonce <- fromIntegral . unNonce . aiCurrentNonce <$>
             wGetAccount wc address False

    txWitnessed <- pure . fixFees (fcMoney feeConfig) $ \fees ->
        let inAcc   = TxInAcc { tiaNonce = nonce, tiaAddr = address }
            inValue = Coin (sum $ map (unCoin . txOutValue) outs) `unsafeAddCoin` unFees fees
            tx      = Tx { txInAcc = inAcc, txInValue = inValue, txOuts = outs }

            signature   = sign secretKey (toTxId tx, publicKey, ())
            witness     = TxWitness   { txwSig = signature, txwPk = publicKey }
            txWitnessed = TxWitnessed { twTx   = tx, twWitness = witness }
        in txWitnessed

    void $ wSubmitTx wc txWitnessed
    sendStateUpdateEvent sendEvent
    return (twTx txWitnessed)

getBalance :: WitnessClient -> Address -> IO (BlocksOrMempool Coin)
getBalance wc address = aiBalances <$> wGetAccount wc address False

getTxHistory :: WitnessClient -> Address -> IO [GTx]
getTxHistory wc address = map tiTx . fromMaybe [] . aiTransactions <$> wGetAccount wc address True

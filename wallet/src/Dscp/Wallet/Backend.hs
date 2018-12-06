{-# LANGUAGE OverloadedLabels #-}

module Dscp.Wallet.Backend
       ( WalletFace(..)
       , createWalletFace
       ) where

import Control.Exception (throwIO)
import Control.Monad.Component (ComponentM, buildComponent_)

import Dscp.Config (option)
import Dscp.Core
import Dscp.Crypto
import Dscp.Wallet.Face
import Dscp.Wallet.KeyStorage
import Dscp.Web
import Dscp.Witness.Logic.Tx
import Dscp.Witness.Web.Client
import Dscp.Witness.Web.Types

type SendEvent = WalletEvent -> IO ()

createWalletFace ::
       HasCoreConfig
    => BaseUrl
    -> SendEvent
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
        , walletGetBalance = getBalance wc sendEvent
        , walletGetTxHistory = getTxHistory wc sendEvent
        }

sendStateUpdateEvent :: SendEvent -> IO ()
sendStateUpdateEvent sendEvent = getAccounts >>= sendEvent . WalletStateUpdateEvent

sendLogEvent :: SendEvent -> Text -> IO ()
sendLogEvent sendEvent = sendEvent . WalletLogEvent

genKeyPair :: SendEvent -> Maybe Text -> Maybe PassPhrase -> IO Account
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

restoreKey :: SendEvent -> Maybe Text -> Encrypted SecretKey -> Maybe PassPhrase -> IO ()
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
    -> SendEvent
    -> Encrypted SecretKey
    -> Maybe PassPhrase
    -> NonEmpty TxOut
    -> IO Tx
sendTx wc sendEvent eSecretKey mPassPhrase (toList -> outs) = do
    secretKey <-
        either throwIO return .
        decrypt (fromMaybe emptyPassPhrase mPassPhrase) $ eSecretKey
    let secretData = mkSecretKeyData secretKey

    nonce <- fromIntegral . unNonce . aiCurrentNonce <$>
             wGetAccount wc (skAddress secretData) False

    let txWitnessed = createTxw (feeConfig ^. option #money) secretData nonce outs

    sendLogEvent sendEvent $
        "Sending transaction: {"
        <> "\n  tx = " <> pretty (twTx txWitnessed)
        <> "\n  witness = " <> pretty (twWitness txWitnessed)
        <> "\n}"
    res <- wSubmitTx wc txWitnessed
    sendLogEvent sendEvent $
        "Transaction result: " <> pretty res

    sendStateUpdateEvent sendEvent
    return (twTx txWitnessed)

getBalance :: WitnessClient -> SendEvent -> Address -> IO (BlocksOrMempool Coin)
getBalance wc sendEvent address = do
    res <- aiBalances <$> wGetAccount wc address False
    sendLogEvent sendEvent $
        "Balances for " <> pretty address <> ": " <> pretty res
    return res

getTxHistory :: WitnessClient -> SendEvent -> Address -> IO [GTx]
getTxHistory wc sendEvent address = do
    res <- map wbiItem . fromMaybe [] . aiTransactions <$> wGetAccount wc address True
    sendLogEvent sendEvent $
        "Tx history for " <> pretty address <> ": " <> pretty (length res) <> " entries"
    return res

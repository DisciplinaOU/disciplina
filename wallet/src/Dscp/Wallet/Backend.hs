module Dscp.Wallet.Backend
       ( WalletFace(..)
       , createWalletFace
       ) where

import Control.Exception (throwIO)
import Dscp.Core (Tx (..), TxInAcc (..), TxWitness (..), TxWitnessed (..), mkAddr, toTxId)
import Dscp.Crypto (decrypt, emptyPassPhrase, encrypt, keyGen, sign, toPublic)

import Dscp.Wallet.Client
import Dscp.Wallet.Face
import Dscp.Wallet.KeyStorage
import Dscp.Web
import Dscp.Witness.Web.Types

createWalletFace :: NetworkAddress -> (WalletEvent -> IO ()) -> IO WalletFace
createWalletFace serverAddress sendEvent = do
    getAccounts >>= sendEvent . WalletStateUpdateEvent
    wc <- createWalletClient serverAddress
    return WalletFace
        { walletGenKeyPair = genKeyPair sendEvent
        , walletListKeys = listKeys
        , walletSendTx = sendTx wc
        , walletGetBalance = getBalance wc
        }

genKeyPair :: (WalletEvent -> IO ()) -> Maybe PassPhrase -> IO Account
genKeyPair sendEvent mPassPhrase = do
    (sk, pk) <- keyGen
    let account = Account
            { accountSecretKey = encrypt (fromMaybe emptyPassPhrase mPassPhrase) sk
            , accountPublicKey = pk
            , accountAddress = mkAddr pk
            }
    addAccount account
    getAccounts >>= sendEvent . WalletStateUpdateEvent
    return account

listKeys :: IO [Account]
listKeys = getAccounts

sendTx :: WalletClient -> Encrypted SecretKey -> Maybe PassPhrase -> NonEmpty TxOut -> IO Tx
sendTx wc eSecretKey mPassPhrase (toList -> outs) = do
    secretKey <- either throwIO return . decrypt (fromMaybe emptyPassPhrase mPassPhrase) $ eSecretKey
    let publicKey = toPublic secretKey
        address = mkAddr publicKey

    -- TODO: request nonce for a given address from witness node
    nonce <- asNextNonce <$> wGetAccountState wc address

    let inAcc = TxInAcc{ tiaNonce = nonce, tiaAddr = address }
        inValue = Coin $ sum $ unCoin . txOutValue <$> outs
        tx = Tx{ txInAcc = inAcc, txInValue = inValue, txOuts = outs }

        signature = sign secretKey (toTxId tx, publicKey)
        witness = TxWitness{ txwSig = signature, txwPk = publicKey }
        txWitnessed = TxWitnessed{ twTx = tx, twWitness = witness }

    void $ wSubmitTx wc txWitnessed
    return tx

getBalance :: WalletClient -> Address -> IO Coin
getBalance wc address = do
    AccountState{..} <- wGetAccountState wc address
    return (bConfirmed asBalances)

module Dscp.Wallet.Backend
       ( WalletFace(..)
       , createWalletFace
       ) where

import Dscp.Core (Tx (..), TxInAcc (..), TxWitness (..), TxWitnessed (..), mkAddr, toTxId)
import Dscp.Crypto (keyGen, sign, toPublic)

import Dscp.Wallet.Client
import Dscp.Wallet.Face
import Dscp.Web
import Dscp.Witness.Web.Types

createWalletFace :: NetworkAddress -> IO WalletFace
createWalletFace serverAddress = do
    wc <- createWalletClient serverAddress
    return WalletFace
        { walletGenKeyPair = genKeyPair
        , walletSendTx = sendTx wc
        , walletGetBalance = getBalance wc
        }

genKeyPair :: IO (SecretKey, PublicKey)
genKeyPair = keyGen

sendTx :: WalletClient -> SecretKey -> NonEmpty TxOut -> IO Tx
sendTx wc secretKey (toList -> outs) = do
  let
    publicKey = toPublic secretKey
    address = mkAddr publicKey

  nonce <- asNextNonce <$> wGetAccountState wc address

  let
    inAcc = TxInAcc{ tiaNonce = nonce, tiaAddr = address }
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

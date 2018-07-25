module Dscp.Wallet.Backend
       ( WalletFace(..)
       , createWalletFace
       ) where

import Dscp.Core (Tx(..), TxInAcc(..), TxWitness(..), TxWitnessed(..), mkAddr, txId)
import Dscp.Crypto (keyGen, sign, toPublic)

import Dscp.Wallet.Face

createWalletFace :: IO WalletFace
createWalletFace = return WalletFace
    { walletGenKeyPair = genKeyPair
    , walletSendTx = sendTx
    , walletGetBalance = getBalance
    }

genKeyPair :: IO (SecretKey, PublicKey)
genKeyPair = keyGen

sendTx :: SecretKey -> NonEmpty TxOut -> IO Tx
sendTx secretKey (toList -> outs) = do
  let
    publicKey = toPublic secretKey
    address = mkAddr publicKey

  -- TODO: request nonce for a given address from witness node
  nonce <- return 1337

  let
    inAcc = TxInAcc{ tiaNonce = nonce, tiaAddr = address }
    inValue = Coin $ sum $ unCoin . txOutValue <$> outs
    tx = Tx{ txInAcc = inAcc, txInValue = inValue, txOuts = outs }

    signature = sign secretKey (txId tx, publicKey)
    witness = TxWitness{ txwSig = signature, txwPk = publicKey }
    _txWitnessed = TxWitnessed{ twTx = tx, twWitness = witness }

  -- TODO: submit txWitnessed to witness node
  return tx

getBalance :: Address -> IO Coin
getBalance _address = do
  -- TODO: request address balance from witness node
  return $ Coin 1337

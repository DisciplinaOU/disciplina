module Dscp.Wallet.Backend
       ( WalletFace(..)
       , createWalletFace
       ) where

import Dscp.Crypto (keyGen)

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
sendTx secretKey outs = error $ "Not implemented: sendTx"
    <> "\n   " <> show secretKey
    <> "\n" <> (unlines . toList $ ("   " <>) . show <$> outs)

getBalance :: Address -> IO Coin
getBalance address = error $ "Not implemented: getBalance"
    <> "\n   " <> show address

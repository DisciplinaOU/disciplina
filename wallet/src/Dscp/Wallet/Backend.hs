module Dscp.Wallet.Backend
       ( WalletFace(..)
       , createWalletFace
       ) where

import Dscp.Wallet.Face

createWalletFace :: IO WalletFace
createWalletFace = return WalletFace
    { walletGenKeyPair = genKeyPair
    , walletSendTx = sendTx
    , walletGetBalance = getBalance
    }

genKeyPair :: PassPhrase -> IO (PublicKey, Encrypted SecretKey)
genKeyPair passPhrase = error $ "Not implemented: genKeyPair"
    <> "\n   " <> show passPhrase

sendTx :: PassPhrase -> Encrypted SecretKey -> NonEmpty TxOut -> IO Tx
sendTx passPhrase secretKey outs = error $ "Not implemented: sendTx"
    <> "\n   " <> show passPhrase
    <> "\n   " <> show secretKey
    <> "\n" <> (unlines . toList $ ("   " <>) . show <$> outs)

getBalance :: Address -> IO Coin
getBalance address = error $ "Not implemented: getBalance"
    <> "\n   " <> show address

module Dscp.Wallet.Face
       ( WalletFace(..)

         -- Re-exports from Dscp
       , Address
       , Coin(..)
       , Encrypted
       , PassPhrase
       , PublicKey
       , SecretKey
       , Tx
       , TxOut(..)
       ) where

import Dscp.Core.Types (Address, Coin(..), Tx, TxOut(..))
import Dscp.Crypto (Encrypted, PassPhrase, PublicKey, SecretKey)

data WalletFace = WalletFace
    { walletGenKeyPair :: IO (SecretKey, PublicKey)
    , walletSendTx :: SecretKey -> NonEmpty TxOut -> IO Tx
    , walletGetBalance :: Address -> IO Coin
    }

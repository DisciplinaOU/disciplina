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
       ) where

import Dscp.Core.Types (Address, Coin(..), Tx)
import Dscp.Crypto (Encrypted, PassPhrase, PublicKey, SecretKey)

data WalletFace = WalletFace
    { walletGenKeyPair :: PassPhrase -> IO (PublicKey, Encrypted SecretKey)
    , walletSendTx :: PassPhrase -> Encrypted SecretKey -> Address -> Coin -> IO Tx
    , walletGetBalance :: Address -> IO Coin
    }

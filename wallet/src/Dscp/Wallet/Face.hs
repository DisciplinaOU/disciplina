module Dscp.Wallet.Face
       ( WalletFace (..)
       , Account (..)
       , WalletEvent (..)

         -- Re-exports from Dscp
       , Address
       , Coin (..)
       , Encrypted
       , PassPhrase
       , PublicKey
       , SecretKey
       , Tx
       , TxOut (..)
       ) where

import Dscp.Core.Types (Address, Coin (..), Tx, TxOut (..))
import Dscp.Crypto (Encrypted, PassPhrase, PublicKey, SecretKey)

data WalletFace = WalletFace
    { walletGenKeyPair :: Maybe PassPhrase -> IO Account
    , walletListKeys :: IO [Account]
    , walletSendTx :: Encrypted SecretKey -> Maybe PassPhrase -> NonEmpty TxOut -> IO Tx
    , walletGetBalance :: Address -> IO Coin
    }

data Account = Account
    { accountSecretKey :: Encrypted SecretKey
    , accountPublicKey :: PublicKey
    , accountAddress :: Address
    }
    deriving (Eq, Show)

data WalletEvent
    = WalletStateUpdateEvent [Account]
    deriving (Eq, Show)
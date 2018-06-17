{-# LANGUAGE CPP #-}

-- | Signature scheme implementations provided by `crytonite` package.

module Disciplina.Crypto.Signing.Cryptonite
       ( CryptoEd25519
       ) where

import Universum

import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.ByteArray (ByteArrayAccess, Bytes)
import qualified Data.ByteString.Lazy as BSL

import Disciplina.Crypto.Hash.Class (AbstractHash (..))
import Disciplina.Crypto.Signing.Class (AbstractPK (..), AbstractSK (..), AbstractSig (..),
                                        HasAbstractSignature (..), SignatureScheme (..))

-- | Tag for 'Ed25519' signature scheme implementation from `crytonite`.
data CryptoEd25519

-- | Default signature scheme implementation for 'ByteArrayAccess'.
instance SignatureScheme CryptoEd25519 where
    type PK CryptoEd25519  = Ed25519.PublicKey
    type SK CryptoEd25519  = Ed25519.SecretKey
    type Sig CryptoEd25519 = Ed25519.Signature

    -- TODO: 'toPublic' isn't free in terms of performance; consider
    -- storing secret key as actual keypair.
    unsafeSignBytes (AbstractSK sk) =
        AbstractSig . Ed25519.sign sk (Ed25519.toPublic sk)

    unsafeVerifyBytes (AbstractPK pk) a (AbstractSig sig) =
        Ed25519.verify pk a sig

-- | Instances for interesting types with 'ByteArrayAccess'
instance HasAbstractSignature CryptoEd25519 ByteString
instance HasAbstractSignature CryptoEd25519 Bytes

#define BA_INSTANCE_SIG(t)                                         \
instance ByteArrayAccess t => HasAbstractSignature CryptoEd25519 t \

BA_INSTANCE_SIG((AbstractHash hf t))
BA_INSTANCE_SIG((AbstractPK ss))
BA_INSTANCE_SIG((AbstractSK ss))
BA_INSTANCE_SIG((AbstractSig ss a))

-- | Separate instance for 'LByteString' (useful for integration with
-- serialisation libs).
instance (SignatureScheme ss, HasAbstractSignature ss ByteString) =>
         HasAbstractSignature ss LByteString where
    unsafeAbstractSign sk = unsafeAbstractSign sk . BSL.toStrict
    unsafeAbstractVerify pk = unsafeAbstractVerify pk . BSL.toStrict

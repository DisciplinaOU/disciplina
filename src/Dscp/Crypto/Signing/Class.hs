{-# LANGUAGE TypeApplications #-}

-- | Abstract interface for a signature scheme

module Dscp.Crypto.Signing.Class
       ( SignatureScheme (..)
       , HasAbstractSignature (..)
       , abstractSign
       , abstractVerify
       , AbstractPK (..)
       , AbstractSK (..)
       , AbstractSig (..)
       ) where

import Universum

import Data.ByteArray (ByteArrayAccess)

-- | Class of signature schemes with defined format of keys and
-- signatures.
class SignatureScheme ss where
    type PK ss  :: *
    type SK ss  :: *
    type Sig ss :: *

    unsafeSignBytes ::
        forall a b. ByteArrayAccess a =>
        AbstractSK ss -> a -> AbstractSig ss b
    unsafeVerifyBytes ::
        forall a b. ByteArrayAccess a =>
        AbstractPK ss -> a -> AbstractSig ss b -> Bool

-- | Wrapper for a public key.
newtype AbstractPK ss = AbstractPK (PK ss)

-- | 'GeneralizedNewtypeDeriving' cannot into type families.
deriving instance Eq (PK ss) => Eq (AbstractPK ss)
deriving instance Ord (PK ss) => Ord (AbstractPK ss)
deriving instance Show (PK ss) => Show (AbstractPK ss)

-- | Wrapper for a secret key. 'Eq', 'Ord' and 'Show' instances
-- are not derived for security reasons.
newtype AbstractSK ss = AbstractSK (SK ss)

-- | Wrapper for a signature. Phantom type parameter 'a' denotes
-- the type of object being signed.
newtype AbstractSig ss a = AbstractSig (Sig ss)

deriving instance Eq (Sig ss) => Eq (AbstractSig ss a)
deriving instance Ord (Sig ss) => Ord (AbstractSig ss a)
deriving instance Show (Sig ss) => Show (AbstractSig ss a)

-- | Provide 'ByteArrayAccess' instances for signatures and keys.
deriving instance ByteArrayAccess (PK ss) =>
    ByteArrayAccess (AbstractPK ss)
deriving instance ByteArrayAccess (SK ss) =>
    ByteArrayAccess (AbstractSK ss)
deriving instance ByteArrayAccess (Sig ss) =>
    ByteArrayAccess (AbstractSig ss a)

-- | For each `a`, provide a way to sign it using scheme `ss`.
class SignatureScheme ss => HasAbstractSignature ss a where
    unsafeAbstractSign ::
        forall b. AbstractSK ss -> a -> AbstractSig ss b
    default unsafeAbstractSign ::
        forall b. ByteArrayAccess a =>
        AbstractSK ss -> a -> AbstractSig ss b
    unsafeAbstractSign = unsafeSignBytes @ss

    unsafeAbstractVerify ::
        forall b. AbstractPK ss -> a -> AbstractSig ss b -> Bool
    default unsafeAbstractVerify ::
        forall b. ByteArrayAccess a =>
        AbstractPK ss -> a -> AbstractSig ss b -> Bool
    unsafeAbstractVerify = unsafeVerifyBytes @ss

-- | Type-safe function for signing.
abstractSign ::
       HasAbstractSignature ss a
    => AbstractSK ss -> a -> AbstractSig ss a
abstractSign = unsafeAbstractSign

-- | Type-safe function for signature verification.
abstractVerify ::
       HasAbstractSignature ss a
    => AbstractPK ss -> a -> AbstractSig ss a -> Bool
abstractVerify = unsafeAbstractVerify

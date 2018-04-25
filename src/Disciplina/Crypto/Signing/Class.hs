
-- | Abstract interface for a signature scheme

module Disciplina.Crypto.Signing.Class
       ( SignatureScheme (..)
       , HasAbstractSignature (..)
       , abstractSign
       , abstractVerify
       , AbstractPK (..)
       , AbstractSK (..)
       , AbstractSig (..)
       ) where

import Universum

-- | Class of signature schemes with defined format of keys and
-- signatures.
class SignatureScheme ss where
    type PK ss  :: *
    type SK ss  :: *
    type Sig ss :: *

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

-- | For each `a`, provide a way to sign it using scheme `ss`.
class SignatureScheme ss => HasAbstractSignature ss a where
    unsafeAbstractSign ::
           forall b. AbstractSK ss -> a -> AbstractSig ss b

    unsafeAbstractVerify ::
           forall b. AbstractPK ss -> a -> AbstractSig ss b -> Bool

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

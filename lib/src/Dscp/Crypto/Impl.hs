{-# LANGUAGE TypeSynonymInstances #-}

-- | Actual implementations of abstract crypto primitives used in Dscp.

module Dscp.Crypto.Impl
       ( -- * Hashing
         HashScheme
       , Hash
       , HasHash
       , hash
       , unsafeHash

         -- * Signing
       , SigScheme
       , PublicKey
       , SecretKey
       , Signature
       , HasSignature
       , sign
       , unsafeSign
       , verify
       , unsafeVerify
       ) where

import Crypto.Hash.Algorithms (Blake2b_256)

import Dscp.Crypto.Hash (AbstractHash (..), CryptoniteFunc, HasAbstractHash (..), abstractHash)
import Dscp.Crypto.Signing (AbstractPK (..), AbstractSK (..), AbstractSig (..), CryptoEd25519,
                            HasAbstractSignature (..), abstractSign, abstractVerify)

------------------------------------------------------
-- Hashing
------------------------------------------------------

-- | We choose `blake2b-256`
type HashScheme = CryptoniteFunc Blake2b_256

type HasHash a = HasAbstractHash HashScheme a
type Hash a = AbstractHash HashScheme a

hash :: forall a. HasHash a => a -> Hash a
hash = abstractHash

unsafeHash :: forall a b. HasHash a => a -> Hash b
unsafeHash = unsafeAbstractHash

------------------------------------------------------
-- Signing
------------------------------------------------------

-- | We choose `ed25519`
type SigScheme = CryptoEd25519

type HasSignature a = HasAbstractSignature SigScheme a
type PublicKey = AbstractPK SigScheme
type SecretKey = AbstractSK SigScheme
type Signature a = AbstractSig SigScheme a

sign :: HasSignature a => SecretKey -> a -> Signature a
sign = abstractSign

unsafeSign :: HasSignature a => SecretKey -> a -> Signature b
unsafeSign = unsafeAbstractSign

verify :: HasSignature a => PublicKey -> a -> Signature a -> Bool
verify = abstractVerify

unsafeVerify :: HasSignature a => PublicKey -> a -> Signature b -> Bool
unsafeVerify = unsafeAbstractVerify

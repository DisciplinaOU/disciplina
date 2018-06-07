{-# LANGUAGE TypeSynonymInstances #-}

-- | Actual implementations of abstract crypto primitives used in Disciplina.

module Disciplina.Crypto.Impl
       ( -- * Hashing
         Hash
       , HasHash
       , hash
       , unsafeHash
         -- * Signing
       , PublicKey
       , SecretKey
       , Signature
       , HasSignature
       , sign
       , unsafeSign
       , verify
       , unsafeVerify
       ) where

import Universum

import Crypto.Hash.Algorithms (Blake2sp_256)

import Disciplina.Crypto.Hash (AbstractHash (..), CryptoniteFunc, HasAbstractHash (..),
                               abstractHash)
import Disciplina.Crypto.Signing (AbstractPK (..), AbstractSK (..), AbstractSig (..), CryptoEd25519,
                                  HasAbstractSignature (..), abstractSign, abstractVerify)

------------------------------------------------------
-- Hashing
------------------------------------------------------

-- | We choose `blake2sp-256`
type HashFunc = CryptoniteFunc Blake2sp_256

type HasHash a = HasAbstractHash HashFunc a
type Hash a = AbstractHash HashFunc a

hash :: forall a. HasHash a => a -> Hash a
hash = abstractHash

unsafeHash :: forall a b. HasHash a => a -> Hash b
unsafeHash = unsafeAbstractHash

------------------------------------------------------
-- Signing
------------------------------------------------------

-- | We choose `ed25519`
type SignatureScheme = CryptoEd25519

type HasSignature a = HasAbstractSignature SignatureScheme a
type PublicKey = AbstractPK SignatureScheme
type SecretKey = AbstractSK SignatureScheme
type Signature a = AbstractSig SignatureScheme a

sign :: HasSignature a => SecretKey -> a -> Signature a
sign = abstractSign

unsafeSign :: HasSignature a => SecretKey -> a -> Signature b
unsafeSign = unsafeAbstractSign

verify :: HasSignature a => PublicKey -> a -> Signature a -> Bool
verify = abstractVerify

unsafeVerify :: HasSignature a => PublicKey -> a -> Signature b -> Bool
unsafeVerify = unsafeAbstractVerify

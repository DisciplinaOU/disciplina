
-- | Specific signature scheme implementations used in Disciplina

module Disciplina.Crypto.Signing.Impl
       ( PublicKey
       , SecretKey
       , Signature
       , sign
       , unsafeSign
       , verify
       , unsafeVerify
       ) where

import Data.ByteArray (ByteArrayAccess)
import Universum

import Disciplina.Crypto.Signing.Class (AbstractPK (..), AbstractSK (..), AbstractSig (..),
                                        SignatureScheme (..), abstractSign, abstractVerify)
import Disciplina.Crypto.Signing.Cryptonite (CryptoEd25519 (..))

type PublicKey = AbstractPK CryptoEd25519
type SecretKey = AbstractSK CryptoEd25519
type Signature a = AbstractSig CryptoEd25519 a

sign :: ByteArrayAccess a => SecretKey -> a -> Signature a
sign = abstractSign

unsafeSign :: ByteArrayAccess a => SecretKey -> a -> Signature b
unsafeSign = unsafeAbstractSign

verify :: ByteArrayAccess a => PublicKey -> a -> Signature a -> Bool
verify = abstractVerify

unsafeVerify :: ByteArrayAccess a => PublicKey -> a -> Signature b -> Bool
unsafeVerify = unsafeAbstractVerify

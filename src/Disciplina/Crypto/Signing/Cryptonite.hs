
-- | Signature scheme implementations provided by `crytonite` package.

module Disciplina.Crypto.Signing.Cryptonite
       ( CryptoEd25519
       ) where

import Universum

import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteString.Lazy as BSL

import Disciplina.Crypto.Signing.Class (AbstractPK (..), AbstractSK (..), AbstractSig (..),
                                        HasAbstractSignature (..), SignatureScheme (..))

-- | Provide 'ByteArrayAccess' instances for signatures and keys.
deriving instance ByteArrayAccess (PK ss) =>
    ByteArrayAccess (AbstractPK ss)
deriving instance ByteArrayAccess (SK ss) =>
    ByteArrayAccess (AbstractSK ss)
deriving instance ByteArrayAccess (Sig ss) =>
    ByteArrayAccess (AbstractSig ss a)

-- | Tag for 'Ed25519' signature scheme implementation from `crytonite`.
data CryptoEd25519

instance SignatureScheme CryptoEd25519 where
    type PK CryptoEd25519  = Ed25519.PublicKey
    type SK CryptoEd25519  = Ed25519.SecretKey
    type Sig CryptoEd25519 = Ed25519.Signature

-- | Sign every datatype with 'ByteArrayAccess'.
instance ByteArrayAccess a => HasAbstractSignature CryptoEd25519 a where
    -- TODO: 'toPublic' isn't free in terms of performance; consider
    -- storing secret key as actual keypair.
    unsafeAbstractSign (AbstractSK sk) =
        AbstractSig . Ed25519.sign sk (Ed25519.toPublic sk)

    unsafeAbstractVerify (AbstractPK pk) a (AbstractSig sig) =
        Ed25519.verify pk a sig

-- | Separate instance for 'LByteString' (useful for integration with
-- serialisation libs).
instance (SignatureScheme ss, HasAbstractSignature ss ByteString) =>
         HasAbstractSignature ss LByteString where
    unsafeAbstractSign sk = unsafeAbstractSign sk . BSL.toStrict
    unsafeAbstractVerify pk = unsafeAbstractVerify pk . BSL.toStrict

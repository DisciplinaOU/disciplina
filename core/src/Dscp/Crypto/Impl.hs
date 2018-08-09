{-# LANGUAGE TypeSynonymInstances #-}

-- | Actual implementations of abstract crypto primitives used in Dscp.

module Dscp.Crypto.Impl
       ( -- * Hashing
         HashScheme
       , Hash
       , HasHash
       , hash
       , unsafeHash
       , unsafeCastHash

         -- * Signing
       , SigScheme
       , PublicKey
       , SecretKey
       , Signature
       , HasSignature
       , toPublic
       , withSeed
       , withIntSeed
       , genSecretKey
       , keyGen
       , sign
       , unsafeSign
       , verify
       , unsafeVerify
       , Signed (..)
       ) where

import Crypto.Error (CryptoFailable (..))
import Crypto.Hash.Algorithms (Blake2b_256)
import Crypto.Random (ChaChaDRG, MonadPseudoRandom, drgNewSeed, seedFromBinary, seedFromInteger,
                      withDRG)
import qualified Data.ByteString as BS
import Fmt (build, (+|), (|+))

import Dscp.Crypto.Hash (AbstractHash (..), CryptoniteFunc, HasAbstractHash (..), abstractHash)
import Dscp.Crypto.Signing (AbstractPK (..), AbstractSK (..), AbstractSig (..), CryptoEd25519,
                            HasAbstractSignature (..), MonadRandom, SignatureScheme (..),
                            abstractSign, abstractVerify)

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

-- | Use hash of one entity verbatim as a hash of another entity.
-- Useful, when we serialise them differently, but require them to have the same hash.
-- Example: using hash of underlying concrete tx as id of generic tx.
unsafeCastHash :: Hash a -> Hash b
unsafeCastHash (AbstractHash hashResult) = AbstractHash hashResult

------------------------------------------------------
-- Signing
------------------------------------------------------

-- | We choose `ed25519`
type SigScheme = CryptoEd25519

type HasSignature a = HasAbstractSignature SigScheme a
type PublicKey = AbstractPK SigScheme
type SecretKey = AbstractSK SigScheme
type Signature a = AbstractSig SigScheme a

-- | Convert secret key to public.
toPublic :: SecretKey -> PublicKey
toPublic = ssToPublic

-- | Seed passed to generator is 40 bytes long. If seed passed is
-- smaller, it'll be repeated until it reaches 40b, otherwise only
-- prefix will be taken.
--
-- It is possible to pass any 'MonadRandom m a' to this method.
withSeed :: ByteString -> MonadPseudoRandom ChaChaDRG a -> a
withSeed seed action = fst $ withDRG (drgNewSeed seed'') action
  where
    len = BS.length seed
    -- Modified seed, if length doesn't match
    seed' = case len of
        0 -> error "withSeed: provided seed is an empty bytestring"
        _ | len > 40 -> BS.take 40 seed
        _ | len < 40 -> let (d,m) = 40 `divMod` len
                        in BS.concat (replicate d seed) `BS.append` (BS.take m seed)
        _ -> seed
    seed'' = case seedFromBinary seed' of
        CryptoPassed x -> x
        CryptoFailed e -> error $ "withSeed: couldn't create seed from binary: " <> show e

-- | Same as 'withSeed', but parametrised over 'Integer'.
withIntSeed :: Integer -> MonadPseudoRandom ChaChaDRG a -> a
withIntSeed seed = fst . withDRG (drgNewSeed $ seedFromInteger seed)

-- | Generate secret key.
genSecretKey :: MonadRandom m => m SecretKey
genSecretKey = ssGenSecret

-- | Generate secret and public key pair.
keyGen :: MonadRandom m => m (SecretKey, PublicKey)
keyGen = (\sk -> (sk, toPublic sk)) <$> genSecretKey

sign :: HasSignature a => SecretKey -> a -> Signature a
sign = abstractSign

unsafeSign :: HasSignature a => SecretKey -> a -> Signature b
unsafeSign = unsafeAbstractSign

verify :: HasSignature a => PublicKey -> a -> Signature a -> Bool
verify = abstractVerify

unsafeVerify :: HasSignature a => PublicKey -> a -> Signature b -> Bool
unsafeVerify = unsafeAbstractVerify

data Signed msg = Signed
    { sgMessage   :: msg
    , sgPublicKey :: PublicKey
    , sgSignature :: Signature msg
    } deriving (Eq, Show, Generic)

instance Buildable msg => Buildable (Signed msg) where
    build Signed{..} = "Signed { sig: " +| build sgSignature |+
                       "; pk: " +| build sgPublicKey |+ " }"

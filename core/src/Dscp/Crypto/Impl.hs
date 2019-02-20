{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Actual implementations of abstract crypto primitives used in Dscp.

module Dscp.Crypto.Impl
       ( -- * Hashing
         HashScheme
       , Hash
       , HasHash
       , hash
       , unsafeHash
       , seedHash
       , unsafeCastHash
       , randomHash

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

         -- * Evenlope with a sign
       , Signed
       , signed
       , unsign

         -- * Other
       , Raw
       ) where

import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (digestFromByteString)
import Crypto.Hash.Algorithms (Blake2b_256)
import Crypto.Hash.IO (hashDigestSize)
import "cryptonite" Crypto.Random (ChaChaDRG, MonadPseudoRandom, drgNewSeed, seedFromBinary,
                                   seedFromInteger, withDRG)
import qualified Data.ByteString as BS
import Fmt (build, (+|), (|+))

import Dscp.Crypto.Hash (AbstractHash (..), CryptoniteFunc, HasAbstractHash (..), abstractHash)
import Dscp.Crypto.Random (MonadRandom (..))
import Dscp.Crypto.Signing (AbstractPK (..), AbstractSK (..), AbstractSig (..), CryptoEd25519,
                            HasAbstractSignature (..), SignatureScheme (..), abstractSign,
                            abstractVerify)

import Dscp.Util

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

seedHash :: Seed ByteString -> Hash a
seedHash (Seed s) = unsafeHash s

-- | Use hash of one entity verbatim as a hash of another entity.
-- Useful, when we serialise them differently, but require them to have the same hash.
-- Example: using hash of underlying concrete tx as id of generic tx.
unsafeCastHash :: Hash a -> Hash b
unsafeCastHash (AbstractHash hashResult) = AbstractHash hashResult

-- | Produce a random hash value.
randomHash :: MonadRandom m => m (Hash a)
randomHash = do
    bs :: ByteString <- getRandomBytes (hashDigestSize (error "untouched" :: Blake2b_256))
    return . AbstractHash $
        digestFromByteString bs ?: error "Wrong algorithm hash / size"

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
                        in BS.concat (replicate d seed) `BS.append` BS.take m seed
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
    , sgSignature :: Signature (Id msg)
    } deriving (Eq, Show, Generic)

-- | Thrown if signed object has invalid signature.
--   Expected to be immidiately catched by client code and somehow rethrown.
data SignatureIsInvalid = SignatureIsInvalid
    deriving (Show)

instance Exception SignatureIsInvalid

signed :: (HasId a, HasSignature (Id a)) => SecretKey -> a -> Signed a
signed sk a = Signed a (toPublic sk) (sign sk (a^.idOf))

unsign :: (MonadThrow m, HasId a, HasSignature (Id a)) => Signed a -> m a
unsign (Signed obj pk sig) =
    if   verify pk (obj^.idOf) sig
    then return obj
    else throwM SignatureIsInvalid

instance Buildable msg => Buildable (Signed msg) where
    build Signed{..} = "Signed { sig: " +| build sgSignature |+
                       "; pk: " +| build sgPublicKey |+ " }"

-- | Type alias for denoting raw bytes. Indended to be used with hashes
-- and signatures, like in type `Hash Raw`, and not type-safe hashing and
-- signing.
-- TODO: probably it makes sense to make it a newtype, like in Cardano?
type Raw = LByteString

{-# LANGUAGE DeriveFunctor #-}

module Dscp.Crypto.JWS
       ( JWitness
       , secretToJwk
       , publicToJwk
       , jwkToSecret
       , jwkToPublic

       , signJWitness
       , verifyJWitness
       ) where

import Control.Lens (AsEmpty, Cons, _Just)
import Crypto.JOSE (CompactJWS, Error, FromCompact (..), HasJwk (jwk), HeaderParam (..), JWK,
                    JWSHeader, KeyMaterial (..), OKPKeyParameters (..), ToCompact (..), asPublicKey,
                    bestJWSAlg, fromKeyMaterial, header, jwkMaterial, newJWSHeader, param, signJWS,
                    signatures, verifyJWS')
import Crypto.Random (MonadRandom (..))

import Dscp.Crypto.Impl
import Dscp.Crypto.Signing.Class
import Dscp.Util

---------------------------------------------------------------------------
-- JSON Web Keys with underlying Disciplina keys
---------------------------------------------------------------------------

-- | Create a JWK containing secret from a secret key.
secretToJwk :: SecretKey -> JWK
secretToJwk = fromKeyMaterial . OKPKeyMaterial . secretToOKPKeyParameters

-- | Helper function to create proper key parameters from a secret key.
secretToOKPKeyParameters :: SecretKey -> OKPKeyParameters
secretToOKPKeyParameters ask'@(AbstractSK sk) = Ed25519Key pk $ Just sk
  where pk = unAbstractPk $ toPublic ask'

-- | Create a public JWK from public key
publicToJwk :: PublicKey -> JWK
publicToJwk = fromKeyMaterial . OKPKeyMaterial . publicToOKPKeyParamerers

-- | Helper function to create proper key parameters from a public key.
publicToOKPKeyParamerers :: PublicKey -> OKPKeyParameters
publicToOKPKeyParamerers (AbstractPK pk) = Ed25519Key pk Nothing

-- | Get a secret key from given JWK
jwkToSecret :: JWK -> Either Text SecretKey
jwkToSecret key = do
    (_, msk) <- keypairFromKeyMaterial $ key ^. jwkMaterial
    maybeToRight "JWK does not contain secret key" msk

-- | Get a public key from given JWK
jwkToPublic :: JWK -> Either Text PublicKey
jwkToPublic = fmap fst . keypairFromKeyMaterial . (^. jwkMaterial)

-- | Get a Ed25519 keypair from JWK, if possible
keypairFromKeyMaterial :: KeyMaterial -> Either Text (PublicKey, Maybe SecretKey)
keypairFromKeyMaterial = \case
    (OKPKeyMaterial okpParams) -> case okpParams of
        Ed25519Key pk msk -> Right (AbstractPK pk, AbstractSK <$> msk)
        X25519Key _ _     -> unexpectedKeyType "X25519"
    (ECKeyMaterial _)  -> unexpectedKeyType "EC"
    (RSAKeyMaterial _) -> unexpectedKeyType "RSA"
    (OctKeyMaterial _) -> unexpectedKeyType "symmetric"
  where unexpectedKeyType keyType = Left ("Expected Ed25519 key, found " <> keyType <> " key")

---------------------------------------------------------------------------
-- JSON Web Signatures
---------------------------------------------------------------------------

-- | Datatype which represents a JWS with payload and corresponding
-- public key included (in "jwk" field).
newtype JWitness = JWitness { unJWitness :: CompactJWS JWSHeader }
    deriving (Eq, Show, ToCompact)

-- Cannot derive 'FromCompact' automatically for some GHC reasons.
instance FromCompact JWitness where
    fromCompact = fmap JWitness . fromCompact

-- | Given a 'SecretKey' and a payload, create valid JWS
-- which includes a public key.
--
-- Note: 'Crypto.JOSE.JWS.signJWS' require 'MonadError' and 'MonadRandom'
-- constraints on runner monad. However, 'MonadRandom' instance is
-- unnecessary for Ed25519 keys, and for those keys 'signJWS' throws an
-- error only if secret key is missing from 'JWK'. As long as we create
-- 'JWK' from 'SecretKey', guaranteeing that such error will not be thrown,
-- this function is really not partial.
signJWitness
    :: Cons s s Word8 Word8
    => SecretKey -> s -> JWitness
signJWitness sk payload =
    JWitness . errToPanic . runFakeRandom . runExceptT $
    signJWS payload $ pure (hdr, key)
  where
    key = secretToJwk sk
    hdr = jwsHeaderWithKey key

-- | Given a JWS with public key included, verify that JWS
-- against included public key and return public key and payload.
verifyJWitness
    :: (Cons s s Word8 Word8, AsEmpty s)
    => JWitness -> Either Text (PublicKey, s)
verifyJWitness witness = do
    key <- getJWitnessKey witness
    pk <- jwkToPublic key
    payload <- first (show @Text @Error) $
        verifyJWS' key (unJWitness witness)
    return (pk, payload)

-- | Creates a new JWS header which contains the correct
-- "alg" field and also "jwk" field containing the public key.
--
-- This particular function assumes that best algorithm
-- for given JWK exists, and panics otherwise. For Ed25519
-- this always works (see source for 'Crypto.JOSE.JWK.bestJWSAlg')
--
-- It also works only for asymmetric keys, for which
-- a public key actually exists (obviously).
--
-- This function also creates only protected headers, because
-- we don't care about unprotected ones.
jwsHeaderWithKey :: JWK -> JWSHeader ()
jwsHeaderWithKey key =
    newJWSHeader ((), alg) &
    jwk .~ Just (HeaderParam () publicKey)
  where
    alg = errToPanic $ bestJWSAlg key
    publicKey = nothingToPanic "jwsHeaderWithKey: symmetric key!" $
        key ^. asPublicKey

-- | Newtype which has not working 'MonadRandom' instance. Only for
-- usage in 'signJWitness'.
newtype FakeRandom a = FakeRandom { unFakeRandom :: Identity a }
    deriving (Functor, Applicative, Monad)

runFakeRandom :: FakeRandom a -> a
runFakeRandom = runIdentity . unFakeRandom

instance MonadRandom FakeRandom where
    getRandomBytes _ = error "panic: FakeRandom is used!"

-- | Given a JWS, fetch the JWK contained inside the "jwk"
-- field, throw and error if there is none.
getJWitnessKey :: JWitness -> Either Text JWK
getJWitnessKey (JWitness jws) =
    maybeToRight "No public key included into JWS" $
    jws ^? signatures.header.jwk._Just.param

-- | Helper function to panic on 'Error'
errToPanic :: Either Error a -> a
errToPanic = leftToPanic . first (show @String)

module Test.Dscp.Crypto.JWS where

import Universum
import Crypto.JOSE (Alg (EdDSA), CompactJWS, Error, HeaderParam (..), JWK, JWSHeader, asPublicKey,
                    jwk, newJWSHeader, signJWS)

import Dscp.Crypto
import Dscp.Util
import Dscp.Util.Test

signBareWithHeader :: JWSHeader () -> SecretKey -> ByteString -> CompactJWS JWSHeader
signBareWithHeader hd sk bs =
    leftToPanic . first (show @String @Error) . withIntSeed 42 . runExceptT $
    signJWS bs (pure $ (hd, secretToJwk sk))

signBare :: SecretKey -> ByteString -> CompactJWS JWSHeader
signBare = signBareWithHeader $ newJWSHeader ((), EdDSA)

signBareWithKey :: JWK -> SecretKey -> ByteString -> CompactJWS JWSHeader
signBareWithKey key = signBareWithHeader $
    newJWSHeader ((), EdDSA) & jwk .~ Just (HeaderParam () key)

spec_JWS :: Spec
spec_JWS = describe "JSON Web Signatures with Disciplina keys" $ do
    it "have the diamond property: `publicToJwk  . toPublic === toPublic . skToJWK`" $ property $
        \(sk :: SecretKey) -> Just (publicToJwk (toPublic sk)) === secretToJwk sk ^. asPublicKey
    it "roundtrip secret keys" $ property $
        \(sk :: SecretKey) -> Right sk === jwkToSecret (secretToJwk sk)
    it "roundtrip public keys" $ property $
        \(pk :: PublicKey) -> Right pk === jwkToPublic (publicToJwk pk)
    it "roundtrips JWS extended with public keys" $ property $
        \(sk :: SecretKey, bs :: ByteString) ->
            verifyJWitness (signJWitness sk bs) === Right (toPublic sk, bs)
    it "fails to verify if `jwk` is absent from the JWS header" $ property $
        \(sk :: SecretKey, bs :: ByteString) ->
            let badJws = JWitness $ signBare sk bs
            in verifyJWitness @ByteString badJws === Left "No public key included into JWS"
    it "fails to verify if included JWK doesn't match the signature" $ property $
        \(sk :: SecretKey, bs :: ByteString, key :: JWK) ->
            let badJws = JWitness $ signBareWithKey key sk bs
            in isLeft $ verifyJWitness @ByteString badJws

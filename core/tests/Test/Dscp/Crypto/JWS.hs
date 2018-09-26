module Test.Dscp.Crypto.JWS where

import Crypto.JOSE.JWK (asPublicKey)

import Dscp.Crypto
import Dscp.Util.Test

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

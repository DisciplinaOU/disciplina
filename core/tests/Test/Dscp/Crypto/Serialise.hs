module Test.Dscp.Crypto.Serialise where

import Universum
import Dscp.Crypto
import Dscp.Util.Test

spec_cryptoSerialise :: Spec
spec_cryptoSerialise = do
    describe "Crypto datatypes binary serialisation" $ do
        describe "Symmetric encryption" $ do
            serialiseRoundtripProp @(Encrypted ByteString)
        describe "Hashing" $ do
            serialiseRoundtripProp @(Hash Raw)
        describe "Signing" $ do
            serialiseRoundtripProp @SecretKey
            serialiseRoundtripProp @PublicKey
            serialiseRoundtripProp @(Signature Raw)
            serialiseRoundtripProp @(MerkleSignature Int)

    describe "Crypto datatypes JSON serialisation" $ do
        aesonRoundtripProp @(MerkleSignature ())

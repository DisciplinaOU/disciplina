module Test.Dscp.Crypto.Serialise where

import Test.Common
import Test.Dscp.Crypto.Instances ()
import Test.Dscp.Serialise (serialiseRoundtripProp)

import Dscp.Crypto (Encrypted, Hash, PublicKey, Raw, SecretKey, Signature)

spec_cryptoSerialise :: Spec
spec_cryptoSerialise = describe "Crypto datatypes binary serialisation" $ do
    describe "Symmetric encryption" $ do
        serialiseRoundtripProp @(Encrypted ByteString)
    describe "Hashing" $ do
        serialiseRoundtripProp @(Hash Raw)
    describe "Signing" $ do
        serialiseRoundtripProp @SecretKey
        serialiseRoundtripProp @PublicKey
        serialiseRoundtripProp @(Signature Raw)

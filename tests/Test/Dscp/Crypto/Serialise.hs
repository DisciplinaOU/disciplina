module Test.Dscp.Crypto.Serialise where

import Test.Common
import Test.Dscp.Crypto.Instances ()
import Test.Dscp.Serialise (serialiseRoundtripProp)

import Dscp.Crypto (Encrypted)

spec_cryptoSerialise :: Spec
spec_cryptoSerialise = describe "Crypto datatypes binary serialisation" $ do
    describe "Symmetric encryption" $ do
        serialiseRoundtripProp @(Encrypted ByteString)

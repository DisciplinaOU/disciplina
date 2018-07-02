module Test.Dscp.Serialisation
    ( spec_Serialisation
    ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Typeable (typeRep)
import Test.Common

import Dscp.Educator.Secret (EducatorSecretJson, KeyfileContent)
import Dscp.Util.Aeson (AsByteString, Base64, Versioned)

spec_Serialisation :: Spec
spec_Serialisation = describe "Serialisation" $ do
    describe "Aeson" $ do
        describe "roundtrip" $ do
            -- Aeson utils
            aesonRoundtripProp @(AsByteString Base64 ByteString)
            aesonRoundtripProp @(Versioned ())

            -- Educator secret
            aesonRoundtripProp @EducatorSecretJson
            aesonRoundtripProp @KeyfileContent

aesonRoundtrip
    :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a)
    => Property
aesonRoundtrip = property $ \(s :: a) -> do
    eitherDecode (encode s) === Right s

aesonRoundtripProp
    :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a, Typeable a)
    => Spec
aesonRoundtripProp =
    it (show (typeRep $ Proxy @a)) $ aesonRoundtrip @a

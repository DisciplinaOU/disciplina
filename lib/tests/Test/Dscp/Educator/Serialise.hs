module Test.Dscp.Educator.Serialise
    ( spec_Serialisation
    ) where

import Test.Common

import Dscp.Resource.Keys (KeyJson (..), KeyfileContent)
import Test.Dscp.Educator.Instances ()
import Test.Dscp.Serialise (aesonRoundtripProp)
import Test.Dscp.Util.Instances ()

spec_Serialisation :: Spec
spec_Serialisation = describe "Serialisation" $ do
    describe "Aeson" $ do
        describe "roundtrip" $ do
            aesonRoundtripProp @KeyJson
            aesonRoundtripProp @KeyfileContent

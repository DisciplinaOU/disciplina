module Test.Dscp.Educator.Serialise
    ( spec_Serialisation
    ) where

import Dscp.Util.Test

import Dscp.Educator.Arbitrary ()
import Dscp.Resource.Keys (KeyJson (..), KeyfileContent)

spec_Serialisation :: Spec
spec_Serialisation = describe "Serialisation" $ do
    describe "Aeson" $ do
        describe "roundtrip" $ do
            aesonRoundtripProp @KeyJson
            aesonRoundtripProp @KeyfileContent

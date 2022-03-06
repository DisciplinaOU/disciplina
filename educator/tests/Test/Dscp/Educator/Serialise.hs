module Test.Dscp.Educator.Serialise
    ( spec_Serialisation
    ) where

import Universum
import Dscp.Util.Test

import Dscp.Educator.Resource (KeyJson (..), KeyfileContent)

spec_Serialisation :: Spec
spec_Serialisation = describe "Serialisation" $ do
    describe "Aeson" $ do
        describe "roundtrip" $ do
            aesonRoundtripProp @KeyJson
            aesonRoundtripProp @KeyfileContent

module Test.Dscp.Util.Serialise
    ( spec_Serialisation
    ) where

import Dscp.Util.Aeson (AsHex, Versioned)
import Dscp.Util.Test

spec_Serialisation :: Spec
spec_Serialisation = describe "Serialisation" $ do
    describe "Aeson" $ do
        describe "roundtrip" $ do
            aesonRoundtripProp @(AsHex ByteString)
            aesonRoundtripProp @(Versioned ())

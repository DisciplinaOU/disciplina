
-- | Utils for roundtrip tests for binary serialisation.

module Test.Dscp.Serialise
    ( serialiseRoundtrip
    , serialiseRoundtripProp
    ) where

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Data.Typeable (typeRep)

import Test.Common

serialiseRoundtrip
    :: forall a. (Arbitrary a, Serialise a, Eq a, Show a)
    => Property
serialiseRoundtrip = property $ \(s :: a) ->
    first (show @Text) (deserialiseOrFail (serialise s)) === Right s

serialiseRoundtripProp
    :: forall a. (Arbitrary a, Serialise a, Eq a, Show a, Typeable a)
    => Spec
serialiseRoundtripProp =
    it (show $ typeRep $ Proxy @a) $ serialiseRoundtrip @a

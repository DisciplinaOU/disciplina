module Test.Dscp.Util.Instances () where

import Test.Common

import Dscp.Util.Aeson (AsByteString (..), Versioned (..))

deriving instance Arbitrary a => Arbitrary (AsByteString encoding a)

instance Arbitrary a => Arbitrary (Versioned a) where
    arbitrary = Versioned <$> arbitrary

module Test.Dscp.Educator.Instances () where

import Test.Common

import Dscp.Educator.Secret (EducatorSecretJson (..))
import Test.Dscp.Crypto.Instances ()

instance Arbitrary EducatorSecretJson where
    arbitrary = EducatorSecretJson <$> arbitrary


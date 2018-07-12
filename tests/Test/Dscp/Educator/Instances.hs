module Test.Dscp.Educator.Instances () where

import Test.Common

import Dscp.Educator.Secret (EducatorSecretJson (..))
import Dscp.Educator.Txs (PrivateTx (..))
import Test.Dscp.Core.Instances ()
import Test.Dscp.Crypto.Instances ()

instance Arbitrary PrivateTx where
    arbitrary = PrivateTx <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary EducatorSecretJson where
    arbitrary = EducatorSecretJson <$> arbitrary

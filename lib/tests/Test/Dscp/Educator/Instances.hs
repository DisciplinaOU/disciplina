module Test.Dscp.Educator.Instances () where

import Test.Common

import Dscp.Educator.Txs (PrivateTx (..))
import Dscp.Resource.Keys (KeyJson (..))
import Test.Dscp.Core.Instances ()
import Test.Dscp.Crypto.Instances ()

instance Arbitrary PrivateTx where
    arbitrary = PrivateTx <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary KeyJson where
    arbitrary = KeyJson <$> arbitrary

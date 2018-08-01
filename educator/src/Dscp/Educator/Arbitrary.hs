-- | Arbitrary instances for educator types.

module Dscp.Educator.Arbitrary where

import Dscp.Core (PrivateTx (..))
import Dscp.Resource.Keys (KeyJson (..))
import Dscp.Util.Test (Arbitrary (..))

instance Arbitrary PrivateTx where
    arbitrary = PrivateTx <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary KeyJson where
    arbitrary = KeyJson <$> arbitrary

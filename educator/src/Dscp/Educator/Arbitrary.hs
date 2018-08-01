-- | Arbitrary instances for educator types.

module Dscp.Educator.Arbitrary where

import Dscp.Core ()
import Dscp.Core.Foundation.Educator.Txs (PrivateTx (..))
import Dscp.Resource.Keys (KeyJson (..))
import Dscp.Util.Test (Arbitrary (..))

instance Arbitrary PrivateTx where
    arbitrary = PrivateTx <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary KeyJson where
    arbitrary = KeyJson <$> arbitrary

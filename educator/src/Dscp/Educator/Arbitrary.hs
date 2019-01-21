-- | Arbitrary instances for educator types.

module Dscp.Educator.Arbitrary where

import Dscp.Resource.Keys (KeyJson (..))
import Dscp.Util.Test

instance Arbitrary KeyJson where
    arbitrary = KeyJson <$> arbitrary

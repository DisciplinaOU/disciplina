module Dscp.Witness.Web.Arbitrary
    (
    ) where

import Dscp.Util
import Dscp.Util.Test
import Dscp.Witness.Web.Types
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

deriving instance Arbitrary a => Arbitrary (Detailed a)

instance (Arbitrary a, Arbitrary (Id a)) => Arbitrary (PaginatedList d a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary BlockList where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (BlocksOrMempool a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (WithBlockInfo a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary AccountInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary HashIs where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary BlockInfo where
    shrink = genericShrink
    arbitrary =
        -- since the type is recursive, cannot derive automatically
        BlockInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                  <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                  <*> arbitrary <*> arbitrary <*> elements [Nothing, Just []]

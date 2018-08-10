module Test.Dscp.Educator.Web.Instances () where

import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import Dscp.Educator.Web.Queries
import Dscp.Educator.Web.Types (IsFinal (..))
import Dscp.Util.Test

deriving instance Arbitrary IsFinal

instance Arbitrary GetAssignmentsFilters where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary GetSubmissionsFilters where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary GetProofsFilters where
    arbitrary = genericArbitrary
    shrink = genericShrink

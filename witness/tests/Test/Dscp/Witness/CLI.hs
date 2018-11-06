module Test.Dscp.Witness.CLI where

import Dscp.Util.Test
import Dscp.Witness.CLI

spec_WitnessCliParamsNoDefaults :: Spec
spec_WitnessCliParamsNoDefaults = describe "Witness node CLI interface" $
    it "should not yield any default values if no CLI params are provided" $
        (runCliArgs witnessConfigParser [] <*> pure mempty) `shouldBe` Just mempty

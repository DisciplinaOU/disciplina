module Test.Dscp.Witness.CLISpec where

import Dscp.Util.Test
import Dscp.Witness.CLI

spec :: Spec
spec = describe "Witness node CLI interface" $
    it "should not yield any default values if no CLI params are provided" $
        (runCliArgs witnessConfigParser [] <*> pure mempty) `shouldBe` Just mempty

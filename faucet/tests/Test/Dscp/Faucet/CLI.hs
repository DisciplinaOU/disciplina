module Test.Dscp.Faucet.CLI where

import Dscp.Faucet.CLI
import Dscp.Util.Test

spec_FaucetCliParamsNoDefaults :: Spec
spec_FaucetCliParamsNoDefaults = describe "Educator CLI interface" $
    it "should not yield any default values if no CLI params are provided" $
        runCliArgs faucetConfigParser [] `shouldBe` Just mempty

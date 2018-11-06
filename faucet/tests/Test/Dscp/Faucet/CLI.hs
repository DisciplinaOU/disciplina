module Test.Dscp.Faucet.CLI where

import Dscp.Faucet.CLI
import Dscp.Util.Test

spec_FaucetCli :: Spec
spec_FaucetCli = describe "Faucet CLI interface" $
    it "should not yield any default values if no CLI params are provided" $
        (runCliArgs faucetConfigParser [] <*> pure mempty) `shouldBe` Just mempty

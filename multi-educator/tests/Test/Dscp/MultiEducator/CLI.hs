module Test.Dscp.MultiEducator.CLI where

import Dscp.MultiEducator.CLI
import Dscp.Util.Test

spec_MultiEducatorCli :: Spec
spec_MultiEducatorCli = describe "MultiEducator CLI interface" $
    it "should not yield any default values if no CLI params are provided" $
        (runCliArgs multiEducatorConfigParser [] <*> pure mempty) `shouldBe` Just mempty

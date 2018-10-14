module Test.Dscp.Educator.CLI where

import Dscp.Educator.CLI
import Dscp.Util.Test

spec_EducatorCli :: Spec
spec_EducatorCli = describe "Educator CLI interface" $
    it "should not yield any default values if no CLI params are provided" $
        (runCliArgs educatorConfigParser [] <*> pure mempty) `shouldBe` Just mempty

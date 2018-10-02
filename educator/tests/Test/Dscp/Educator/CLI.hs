module Test.Dscp.Educator.CLI where

import Dscp.Educator.CLI
import Dscp.Util.Test

spec_EducatorCliParamsNoDefaults :: Spec
spec_EducatorCliParamsNoDefaults = describe "Educator CLI interface" $
    it "should not yield any default values if no CLI params are provided" $
        runCliArgs educatorConfigParser [] `shouldBe` Just mempty

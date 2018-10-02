module Test.Dscp.Witness.CLISpec where

import Options.Applicative (Parser, defaultPrefs, execParserPure, getParseResult, info)

import Dscp.Util.Test
import Dscp.Witness.CLI

runArgs :: Parser a -> [String] -> Maybe a
runArgs p = getParseResult . execParserPure defaultPrefs (info p mempty)

spec :: Spec
spec = describe "Witness node CLI interface" $
    it "should not yield any default values if no CLI params are provided" $
        runArgs witnessConfigParser [] `shouldBe` Just mempty

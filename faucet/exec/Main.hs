-- | Starting point for running a Faucet

module Main where

import Fmt ((+|), (|+))
import Loot.Base.HasLens (lensOf)
import Loot.Log (logInfo)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI (versionOption)
import Dscp.Config (buildConfig, configParamsParser)
import Dscp.Core
import Dscp.Faucet
import Dscp.Resource.Keys
import Dscp.Util.Aeson ()

main :: IO ()
main = do
    fConfig <- getFaucetConfig
    launchFaucetRealMode fConfig $ do
        printSourceInfo
        serveFaucetAPIReal
  where
    printSourceInfo = do
        pk <- view $ lensOf @(KeyResources FaucetApp) . krPublicKey
        let addr = mkAddr pk
        logInfo $ "Faucet source address: " +| addr |+ ""

getFaucetConfig :: IO FaucetConfigRec
getFaucetConfig = do
    let parser = (,) <$> configParamsParser <*> faucetConfigParser
    (configParams, cliConfig) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Disciplina faucet node."
    buildConfig configParams $
        fmap (<> cliConfig) . fillFaucetConfig

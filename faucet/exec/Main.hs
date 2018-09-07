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

main :: IO ()
main = do
    (faucetParams, faucetConfig) <- getFaucetParams
    launchFaucetRealMode faucetConfig faucetParams $ do
        printSourceInfo
        serveFaucetAPIReal (_fpWebParams faucetParams)
  where
    printSourceInfo = do
        pk <- view $ lensOf @(KeyResources FaucetApp) . krPublicKey
        let addr = mkAddr pk
        logInfo $ "Faucet source address: " +| addr |+ ""

getFaucetParams :: IO (FaucetParams, FaucetConfigRec)
getFaucetParams = do
    let parser = (,) <$> faucetParamsParser <*> configParamsParser
    (params, configPath) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Disciplina faucet node."
    config <- buildConfig configPath fillFaucetConfig
    return (params, config)

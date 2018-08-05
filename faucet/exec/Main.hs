-- | Starting point for running a Faucet

module Main where

import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI (versionOption)
import Dscp.Config (buildConfig, configParamsParser)
import Dscp.Faucet

main :: IO ()
main = do
    (faucetParams, faucetConfig) <- getFaucetParams
    launchFaucetRealMode faucetConfig faucetParams $ do
        serveFaucetAPIReal (_fpWebParams faucetParams)

getFaucetParams :: IO (FaucetParams, FaucetConfigRec)
getFaucetParams = do
    let parser = (,) <$> faucetParamsParser <*> configParamsParser
    (params, configPath) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Disciplina faucet node."
    config <- buildConfig configPath fillFaucetConfig
    return (params, config)

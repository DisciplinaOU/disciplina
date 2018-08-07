-- | Starting point for running a Faucet

module Main where

import Loot.Base.HasLens (lensOf)
import Loot.Log (logError)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI (versionOption)
import Dscp.Config (buildConfig, configParamsParser)
import Dscp.Faucet
import Dscp.Witness.Web

main :: IO ()
main = do
    (faucetParams, faucetConfig) <- getFaucetParams
    launchFaucetRealMode faucetConfig faucetParams $ do
        pingBackend
        serveFaucetAPIReal (_fpWebParams faucetParams)
  where
    pingBackend = do
        wc <- view (lensOf @WitnessClient)
        liftIO (wPing wc)
            `onException` logError "Failled to connect witness node"

getFaucetParams :: IO (FaucetParams, FaucetConfigRec)
getFaucetParams = do
    let parser = (,) <$> faucetParamsParser <*> configParamsParser
    (params, configPath) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Disciplina faucet node."
    config <- buildConfig configPath fillFaucetConfig
    return (params, config)

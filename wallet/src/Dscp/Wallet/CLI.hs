module Dscp.Wallet.CLI
       ( getWalletConfig
       , getCLIWalletConfig
       ) where

import Options.Applicative (execParser, fullDesc, help, helper, info, long, metavar, optional,
                            progDesc, strOption, Parser)

import Loot.Config (OptModParser, (.::), (.:<))
import Dscp.CommonCLI
import Dscp.Config
import Dscp.Wallet.Config

knitCommandParser :: Parser (Maybe Text)
knitCommandParser = optional $ strOption $
    long "knit" <>
    metavar "COMMAND" <>
    help "Execute provided knit command and exit."

walletConfigParser :: OptModParser WalletConfig
walletConfigParser = #wallet .:<
    (#witness .:: clientAddressParser "witness"
                  "Address of a witness node to communicate with")

getWalletConfig :: IO WalletConfigRec
getWalletConfig = do
    let parser = (,) <$> configParamsParser <*> walletConfigParser
    (configParams, cliConfigMod) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Ariadne wallet for Disciplina"
    let wrapConfig cfg = cliConfigMod $ defaultWalletConfig <> cfg
    buildConfig configParams $
        fmap wrapConfig . fillWalletConfig

getCLIWalletConfig :: IO (WalletConfigRec, Maybe Text)
getCLIWalletConfig = do
    let parser = (,,) <$> configParamsParser <*>
                          walletConfigParser <*>
                          knitCommandParser
    (configParams, cliConfigMod, knitCommand) <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Ariadne cli wallet for Disciplina"
    let wrapConfig cfg = cliConfigMod $ defaultWalletConfig <> cfg
    configRec <- buildConfig configParams $
        fmap wrapConfig . fillWalletConfig
    return (configRec, knitCommand)

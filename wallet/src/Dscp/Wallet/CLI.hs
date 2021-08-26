module Dscp.Wallet.CLI
       ( WalletCLIParams (..)
       , getWalletCLIParams
       ) where

import Options.Applicative (execParser, fullDesc, help, helper, info, long, metavar, optional,
                            progDesc, strOption)

import Dscp.CommonCLI
import Dscp.Config
import Dscp.Web

data WalletCLIParams = WalletCLIParams
    { wpWitness      :: BaseUrl
    , wpKnitCommand  :: Maybe Text
    , wpConfigParams :: ConfigParams
    }

getWalletCLIParams :: IO WalletCLIParams
getWalletCLIParams = do
    let parser = do
            wpWitness <- clientAddressParser "witness" "Address of a witness node to communicate with"
            wpKnitCommand <- optional $ strOption $
                long "knit" <>
                metavar "COMMAND" <>
                help "Execute provided knit command and exit."
            wpConfigParams <- configParamsParser
            return WalletCLIParams{..}
    execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Ariadne wallet for Disciplina"

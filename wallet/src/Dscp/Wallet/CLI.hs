module Dscp.Wallet.CLI
       ( BaseUrl
       , getWalletCLIParams
       ) where

import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI
import Dscp.Web

getWalletCLIParams :: IO BaseUrl
getWalletCLIParams = do
    let parser = clientAddressParser "witness" "Address of a witness node to communicate with"
    execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Ariadne wallet for Disciplina"

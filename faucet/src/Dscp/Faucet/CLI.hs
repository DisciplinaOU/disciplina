{-# LANGUAGE ApplicativeDo #-}

module Dscp.Faucet.CLI
    ( faucetParamsParser
    ) where

import Options.Applicative (Parser, help, long, metavar, option)

import Dscp.CommonCLI
import Dscp.Faucet.Launcher.Params

translatedAmountParser :: Parser TranslatedAmount
translatedAmountParser = option (TranslatedAmount <$> coinReadM) $
    long "translated-amount" <>
    metavar "COINS" <>
    help "How much money to send on each request to faucet"

faucetParamsParser :: Parser FaucetParams
faucetParamsParser = do
    _fpLoggingParams <- logParamsParser "faucet"
    _fpKeyParams <- baseKeyParamsParser "faucet"
    _fpWebParams <- serverParamsParser "faucet"
    _fpWitnessAddress <- networkAddressParser "witness-backend" witnessBackendHelp
    _fpTranslatedAmount <- translatedAmountParser
    return FaucetParams{..}
  where
    witnessBackendHelp = "Address of witness node to accept faucet transactions"

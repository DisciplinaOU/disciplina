{-# LANGUAGE ApplicativeDo #-}

module Dscp.Faucet.CLI
    ( faucetParamsParser
    ) where

import Options.Applicative (Parser, help, long, metavar, option, switch)

import Dscp.CommonCLI
import Dscp.Faucet.Launcher.Params

translatedAmountParser :: Parser TranslatedAmount
translatedAmountParser = option (TranslatedAmount <$> coinReadM) $
    long "translated-amount" <>
    metavar "COINS" <>
    help "How much money to send on each request to faucet"

dryRunParser :: Parser DryRun
dryRunParser = fmap DryRun . switch $
    long "dry-run" <>
    help "Do not communicate with witness backend"

faucetParamsParser :: Parser FaucetParams
faucetParamsParser = do
    _fpLoggingParams <- logParamsParser "faucet"
    _fpKeyParams <- baseKeyParamsParser "faucet"
    _fpWebParams <- serverParamsParser "faucet"
    _fpWitnessAddress <- clientAddressParser "witness-backend" witnessBackendHelp
    _fpTranslatedAmount <- translatedAmountParser
    _fpDryRun <- dryRunParser
    _fpAppDirParam <- appDirParamParser
    return FaucetParams{..}
  where
    witnessBackendHelp = "Address of witness node to accept faucet transactions"

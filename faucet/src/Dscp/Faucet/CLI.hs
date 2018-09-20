{-# LANGUAGE ApplicativeDo #-}

module Dscp.Faucet.CLI
    ( translatedAmountParser
    , dryRunParser
    ) where

import Options.Applicative (Parser, help, long, metavar, option, switch)

import Dscp.CommonCLI
import Dscp.Faucet.Config

translatedAmountParser :: Parser TransferredAmount
translatedAmountParser = option (TransferredAmount <$> coinReadM) $
    long "translated-amount" <>
    metavar "COINS" <>
    help "How much money to send on each request to faucet"

dryRunParser :: Parser DryRun
dryRunParser = fmap DryRun . switch $
    long "dry-run" <>
    help "Do not communicate with witness backend"

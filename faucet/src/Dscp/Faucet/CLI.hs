{-# LANGUAGE OverloadedLabels #-}

module Dscp.Faucet.CLI
    ( faucetConfigParser
    ) where

import Loot.Config (OptParser, (.::), (.:<), (.<>))
import Options.Applicative (Parser, flag', help, long, metavar, option)

import Dscp.CommonCLI
import Dscp.Faucet.Config

transferredAmountParser :: Parser TransferredAmount
transferredAmountParser = option (TransferredAmount <$> coinReadM) $
    long "translated-amount" <>
    metavar "COINS" <>
    help "How much money to send on each request to faucet"

dryRunParser :: Parser DryRun
dryRunParser = fmap DryRun . flag' True $
    long "dry-run" <>
    help "Do not communicate with witness backend"

faucetConfigParser :: OptParser FaucetConfig
faucetConfigParser = #faucet .:<
    (#logging .:: logParamsParser "faucet" .<>
     #keys .:: baseKeyParamsParser "faucet" .<>
     #api .:: serverParamsParser "faucet" .<>
     #witnessBackend .:: clientAddressParser "witness-backend" wbHelp .<>
     #transferredAmount .:: transferredAmountParser .<>
     #dryRun .:: dryRunParser .<>
     #appDir .:: appDirParamParser)
  where
    wbHelp = "Address of witness node to accept faucet transactions"

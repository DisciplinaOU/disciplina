-- | CLI parameters of witness.

module Dscp.Witness.CLI
    ( witnessParamsParser
    ) where

import Universum

import Options.Applicative (Parser)

import Dscp.CLI.Common (logParamsParser, rocksParamsParser)
import Dscp.Witness.Launcher.Params (WitnessParams (..))

witnessParamsParser :: Parser WitnessParams
witnessParamsParser =
    WitnessParams <$> logParamsParser "witness" <*> rocksParamsParser

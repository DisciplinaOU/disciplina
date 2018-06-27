{-# LANGUAGE ApplicativeDo #-}

-- | CLI parameters of witness.

module Dscp.Witness.CLI
    ( witnessParamsParser
    ) where

import Universum

import Options.Applicative (Parser)

import Dscp.CLI.Common (logParamsParser, netServParamsParser, rocksParamsParser)
import Dscp.Witness.Launcher.Params (WitnessParams (..))

witnessParamsParser :: Parser WitnessParams
witnessParamsParser = do
    wpLoggingParams <- logParamsParser "witness"
    wpDBParams <- rocksParamsParser
    wpNetworkParams <- netServParamsParser
    pure $ WitnessParams {..}

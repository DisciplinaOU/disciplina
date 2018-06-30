{-# LANGUAGE ApplicativeDo #-}

-- | CLI for educator.

module Dscp.Educator.CLI
    ( educatorParamsParser
    ) where

import Options.Applicative (Parser)

import Dscp.CLI (sqliteParamsParser)
import Dscp.Educator.Launcher.Params (EducatorParams (..))
import Dscp.Witness.CLI (witnessParamsParser)

educatorParamsParser :: Parser EducatorParams
educatorParamsParser = do
    epWitnessParams <- witnessParamsParser
    epDBParams <- sqliteParamsParser
    return EducatorParams{..}

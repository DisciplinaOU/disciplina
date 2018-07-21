{-# LANGUAGE ApplicativeDo #-}

-- | CLI for educator.

module Dscp.Educator.CLI
    ( educatorParamsParser
    ) where

import Options.Applicative (Parser)

import Dscp.CLI (networkAddressParser, sqliteParamsParser)
import Dscp.CLI.Common (keyParamsParser)
import Dscp.Educator.Launcher.Params (EducatorParams (..))
import Dscp.Educator.Web.Params (EducatorWebParams (..))
import Dscp.Witness.CLI (witnessParamsParser)

educatorWebParamsParser :: Parser EducatorWebParams
educatorWebParamsParser = do
    ewpStudentApiAddr <- networkAddressParser "student-listen"
        "Host/port for serving Student API"
    return EducatorWebParams{..}

educatorParamsParser :: Parser EducatorParams
educatorParamsParser = do
    epWitnessParams <- witnessParamsParser
    epDBParams <- sqliteParamsParser
    epKeyParams <- keyParamsParser "educator"
    epWebParams <- educatorWebParamsParser
    return EducatorParams{..}

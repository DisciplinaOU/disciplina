{-# LANGUAGE ApplicativeDo #-}

-- | CLI for educator.

module Dscp.Educator.CLI
    ( educatorParamsParser
    ) where

import Options.Applicative (Parser, help, long, metavar, strOption, value)

import Dscp.CommonCLI (keyParamsParser)
import Dscp.DB.SQLite (SQLiteDBLocation (..), SQLiteParams (..))
import Dscp.Educator.Launcher.Params (EducatorParams (..))
import Dscp.Educator.Web.Params (EducatorWebParams (..))
import Dscp.Witness.CLI (networkAddressParser, witnessParamsParser)

sqliteParamsParser :: Parser SQLiteParams
sqliteParamsParser = fmap (SQLiteParams . SQLiteReal) $ strOption $
    long "sql-path" <>
    metavar "FILEPATH" <>
    help "Path to database directory for educator's private data." <>
    value "educator-db"

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

{-# LANGUAGE ApplicativeDo #-}

-- | CLI for educator.

module Dscp.Educator.CLI
    ( educatorParamsParser
    ) where

import Options.Applicative (Parser, help, long, metavar, strOption, value)

import Dscp.CommonCLI (keyParamsParser)
import Dscp.CommonCLI (serverParamsParser)
import Dscp.DB.SQLite (SQLiteDBLocation (..), SQLiteParams (..))
import Dscp.Educator.Launcher.Params (EducatorParams (..))
import Dscp.Witness.CLI (witnessParamsParser)

sqliteParamsParser :: Parser SQLiteParams
sqliteParamsParser = fmap (SQLiteParams . SQLiteReal) $ strOption $
    long "sql-path" <>
    metavar "FILEPATH" <>
    help "Path to database directory for educator's private data." <>
    value "educator-db"

educatorParamsParser :: Parser EducatorParams
educatorParamsParser = do
    epWitnessParams <- witnessParamsParser
    epDBParams <- sqliteParamsParser
    epKeyParams <- keyParamsParser "educator"
    epWebParams <- serverParamsParser "Student"
    return EducatorParams{..}

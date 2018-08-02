{-# LANGUAGE ApplicativeDo #-}

-- | CLI for educator.

module Dscp.Educator.CLI
    ( educatorParamsParser
    ) where

import Options.Applicative (Parser, help, long, metavar, strOption, switch, value)

import Dscp.CommonCLI (baseKeyParamsParser, serverParamsParser)
import Dscp.DB.SQLite (SQLiteDBLocation (..), SQLiteParams (..))
import Dscp.Educator.Launcher.Params (EducatorParams (..))
import Dscp.Educator.Web.Params (EducatorWebParams (..))
import Dscp.Resource.Keys (EducatorKeyParams (..))
import Dscp.Witness.CLI (witnessParamsParser)

sqliteParamsParser :: Parser SQLiteParams
sqliteParamsParser = fmap (SQLiteParams . SQLiteReal) $ strOption $
    long "sql-path" <>
    metavar "FILEPATH" <>
    help "Path to database directory for educator's private data." <>
    value "educator-db"

educatorWebParamsParser :: Parser EducatorWebParams
educatorWebParamsParser = do
    ewpServerParams <- serverParamsParser "Student"
    ewpWithBot <- switch $
        long "educator-bot" <>
        help "Enable bot which would automatically react on student actions."
    return EducatorWebParams{..}

educatorKeyParamsParser :: Parser EducatorKeyParams
educatorKeyParamsParser =
    EducatorKeyParams <$> baseKeyParamsParser "educator"

educatorParamsParser :: Parser EducatorParams
educatorParamsParser = do
    epWitnessParams <- witnessParamsParser
    epDBParams <- sqliteParamsParser
    epKeyParams <- educatorKeyParamsParser
    epWebParams <- educatorWebParamsParser
    return EducatorParams{..}

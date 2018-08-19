{-# LANGUAGE ApplicativeDo #-}

-- | CLI for educator.

module Dscp.Educator.CLI
    ( educatorParamsParser
    ) where

import Options.Applicative (Parser, help, long, metavar, option, strOption, switch, value)

import Dscp.CommonCLI (baseKeyParamsParser, serverParamsParser, timeReadM, appDirParamParser)
import Dscp.DB.SQLite (SQLiteDBLocation (..), SQLiteParams (..))
import Dscp.Educator.Launcher.Params (EducatorKeyParams (..), EducatorParams (..))
import Dscp.Educator.Web.Bot.Params (EducatorBotParams (..), EducatorBotSwitch (..))
import Dscp.Educator.Web.Params (EducatorWebParams (..))
import Dscp.Witness.CLI (witnessParamsParser)

sqliteParamsParser :: Parser SQLiteParams
sqliteParamsParser = fmap (SQLiteParams . SQLiteReal) $ strOption $
    long "sql-path" <>
    metavar "FILEPATH" <>
    help "Path to database directory for educator's private data." <>
    value "educator-db"

educatorBotParamsParser :: Parser EducatorBotSwitch
educatorBotParamsParser = do
    enabled <- switch $
        long "educator-bot" <>
        help "Enable bot which would automatically react on student actions."
    ebpSeed <- strOption $
        long "educator-bot-seed" <>
        metavar "TEXT" <>
        help "Seed for bot pregenerated data" <>
        value "Memes generator"
    ebpOperationsDelay <- option timeReadM $
        long "educator-bot-delay" <>
        metavar "TIME" <>
        help "Delay before user action and bot reaction on it" <>
        value 0
    return $
      if enabled
        then EducatorBotOn EducatorBotParams{..}
        else EducatorBotOff

educatorWebParamsParser :: Parser EducatorWebParams
educatorWebParamsParser = do
    ewpServerParams <- serverParamsParser "Student"
    ewpBotParams <- educatorBotParamsParser
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
    epAppDirParam <- appDirParamParser
    return EducatorParams{..}

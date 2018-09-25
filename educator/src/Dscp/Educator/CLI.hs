{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLabels #-}

-- | CLI for educator.

module Dscp.Educator.CLI
    ( educatorConfigParser
    ) where

import Loot.Config (OptParser, upcast, (.::), (.:<), (.<>))
import Options.Applicative (Parser, auto, flag', help, long, metavar, option, strOption, switch,
                            value)

import Dscp.CommonCLI
import Dscp.DB.SQLite
import Dscp.Educator.Config
import Dscp.Educator.Launcher.Params (EducatorKeyParams (..))
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Bot.Params (EducatorBotParams (..), EducatorBotSwitch (..))
import Dscp.Educator.Web.Params (EducatorWebParams (..))
import Dscp.Witness.CLI (witnessConfigParser)

sqliteParamsParser :: Parser SQLiteParams
sqliteParamsParser = do
    srpPath <- strOption $
        long "sql-path" <>
        metavar "FILEPATH" <>
        help "Path to database directory for educator's private data." <>
        value "educator-db"
    srpConnNum <- optional . option auto $
        long "sql-conns" <>
        metavar "INTEGER" <>
        help "Connection pool size, i.e. number of threads which can perform \
             \SQL requests in parallel. By default (cores - 1) number is used, \
             \where 'cores' is number of processor cores (or number of GHC \
             \cabapilities, if it was set manually)."
    srpMaxPending <- option auto $
        long "sql-max-pending" <>
        metavar "INTEGER" <>
        help "Maximal number of threads waiting for free connection in pool." <>
        value 200
    return $ SQLiteParams $ SQLiteReal SQLiteRealParams{..}

educatorBotParamsParser :: Parser EducatorBotSwitch
educatorBotParamsParser = do
    enabled <- switch $
        long "educator-bot" <>
        help "Enable bot which would automatically react on student actions."
    ebpSeed <- strOption $
        long "educator-bot-seed" <>
        metavar "TEXT" <>
        help "Seed for bot pregenerated data." <>
        value "Memes generator"
    ebpOperationsDelay <- option timeReadM $
        long "educator-bot-delay" <>
        metavar "TIME" <>
        help "Delay before user action and bot reaction on it." <>
        value 0
    return $
      if enabled
        then EducatorBotOn EducatorBotParams{..}
        else EducatorBotOff

noAuthContextParser :: Parser (NoAuthData s) -> Parser (NoAuthContext s)
noAuthContextParser dataParser = do
    maybe NoAuthOffContext NoAuthOnContext <$> optional dataParser

educatorWebParamsParser :: Parser EducatorWebParams
educatorWebParamsParser = do
    ewpServerParams <- serverParamsParser "Educator"
    ewpBotParams <- educatorBotParamsParser
    ewpEducatorAPINoAuth <- noAuthContextParser . flag' () $
        long "educator-api-no-auth" <>
        help "Make authentication into Educator API optional."
    ewpStudentAPINoAuth <- noAuthContextParser . option addressReadM $
        long "student-api-no-auth" <>
        metavar "ADDRESS" <>
        help "Make authentication into Student API optional. \
             \Requires id of student which is pretended as request author. \
             \You can still provide authentication header to specify request \
             \author, if invalid data is passed authentication will \
             \automatically roll back to no-auth scheme."
    return EducatorWebParams{..}

educatorKeyParamsParser :: Parser EducatorKeyParams
educatorKeyParamsParser =
    EducatorKeyParams <$> baseKeyParamsParser "educator"

educatorConfigParser :: OptParser EducatorConfig
educatorConfigParser =
    fmap upcast witnessConfigParser .<>
    #educator .:<
        (#db .:: sqliteParamsParser .<>
         #keys .:: educatorKeyParamsParser .<>
         #api .:: educatorWebParamsParser)

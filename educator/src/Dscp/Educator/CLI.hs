{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLabels #-}

-- | CLI for educator.

module Dscp.Educator.CLI
    ( sqliteParamsParser
    , educatorWebConfigParser
    , educatorConfigParser
    , publishingPeriodParser
    ) where

import Control.Lens (iso)
import Loot.Config (ModParser, OptModParser, uplift, (%::), (..:), (.::), (.:<), (<*<))
import Options.Applicative (Parser, auto, flag', help, long, metavar, option, strOption)
import Time (Second, Time)

import Dscp.CommonCLI
import Dscp.DB.SQLite
import Dscp.Educator.Config
import Dscp.Educator.Launcher.Params (EducatorKeyParams (..))
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Bot.Params
import Dscp.Educator.Web.Config
import Dscp.Witness.CLI (witnessConfigParser)

sqliteParamsParser :: ModParser SQLiteParams
sqliteParamsParser = over (sdpModeL._SQLiteReal) <$>
    srpPathL       ..: pathParser <*<
    srpConnNumL    ..: connNumParser <*<
    srpMaxPendingL ..: maxPendingParser
  where
    pathParser = strOption $
        long "sql-path" <>
        metavar "FILEPATH" <>
        help "Path to database directory for educator's private data. If not \
             \specified, 'educator-db' directory is used."
    connNumParser = fmap Just . option auto $
        long "sql-conns" <>
        metavar "INTEGER" <>
        help "Connection pool size, i.e. number of threads which can perform \
             \SQL requests in parallel. By default (cores - 1) number is used, \
             \where 'cores' is number of processor cores (or number of GHC \
             \capabilities, if it was set manually)."
    maxPendingParser = option auto $
        long "sql-max-pending" <>
        metavar "INTEGER" <>
        help "Maximal number of threads waiting for free connection in pool."

educatorBotParamsParser :: ModParser EducatorBotParams
educatorBotParamsParser =
    ebpEnabledL         ..: enabledParser <*<
    ebpSeedL            ..: seedParser <*<
    ebpOperationsDelayL ..: delayParser
  where
    enabledParser = flag' True $
        long "educator-bot" <>
        help "Enable bot which would automatically react on student actions."
    seedParser = strOption $
        long "educator-bot-seed" <>
        metavar "TEXT" <>
        help "Seed for bot pregenerated data."
    delayParser = option timeReadM $
        long "educator-bot-delay" <>
        metavar "TIME" <>
        help "Delay before user action and bot reaction on it."

noAuthContextParser :: Parser (NoAuthData s) -> Parser (NoAuthContext s)
noAuthContextParser = fmap NoAuthOnContext

educatorApiNoAuthParser :: Parser (NoAuthContext "educator")
educatorApiNoAuthParser = noAuthContextParser . flag' () $
    long "educator-api-no-auth" <>
    help "Make authentication into Educator API optional."

studentApiNoAuthParser :: Parser (NoAuthContext "student")
studentApiNoAuthParser = noAuthContextParser . option addressReadM $
    long "student-api-no-auth" <>
    metavar "ADDRESS" <>
    help "Make authentication into Student API optional. \
         \Requires id of student which is pretended as request author. \
         \You can still provide authentication header to specify request \
         \author, if invalid data is passed authentication will \
         \automatically roll back to no-auth scheme."

educatorKeyParamsParser :: ModParser EducatorKeyParams
educatorKeyParamsParser =
    over (iso unEducatorKeyParams EducatorKeyParams) <$>
    baseKeyParamsParser "educator"

publishingPeriodParser :: Parser (Time Second)
publishingPeriodParser = option timeReadM $
    long "publication-period" <>
    metavar "TIME" <>
    help "How often grades should be dumped to private blocks and submitted to \
         \public chain. Block creation may be skipped if there are no relevant \
         \changes, in this case node will wait for a whole cycle before trying \
         \to create a block next time."

educatorWebConfigParser :: OptModParser EducatorWebConfig
educatorWebConfigParser =
    #serverParams .:: serverParamsParser "Educator" <*<
    #botParams %:: educatorBotParamsParser <*<
    #educatorAPINoAuth .:: educatorApiNoAuthParser <*<
    #studentAPINoAuth .:: studentApiNoAuthParser

educatorConfigParser :: OptModParser EducatorConfig
educatorConfigParser =
    uplift witnessConfigParser <*<
    #educator .:<
        (#db %:: sqliteParamsParser <*<
         #keys %:: educatorKeyParamsParser <*<
         #api .:< educatorWebConfigParser <*<
         #publishing .:<
            (#period .:: publishingPeriodParser)
        )


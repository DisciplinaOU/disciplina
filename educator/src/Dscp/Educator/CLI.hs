{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLabels #-}

-- | CLI for educator.

module Dscp.Educator.CLI
    ( postgresParamsParser
    , educatorBotConfigParser
    , noAuthContextParser
    , educatorApiNoAuthParser
    , studentApiNoAuthParser
    , educatorWebConfigParser
    , educatorConfigParser
    , publishingPeriodParser
    , pdfResourcesPathParser
    ) where

import Control.Arrow (left)
import Loot.Config (OptModParser, (.:+), (.:-), (.::), (.:<), (<*<))
import Options.Applicative (Parser, auto, eitherReader, flag', help, long, metavar, option,
                            strOption)
import Time (Second, Time)
import Universum

import Dscp.CommonCLI
import Dscp.Core.PubChain (PubAddress, pubAddrFromText)
import Dscp.Core.Foundation.Educator (ItemDesc (..), toItemDesc)
import Dscp.DB.SQL
import Dscp.Educator.Config
import Dscp.Educator.Web.Auth
import Dscp.Educator.Web.Bot.Params
import Dscp.Educator.Web.Config
-- import Dscp.Witness.CLI (witnessConfigParser)

postgresParamsParser :: OptModParser PostgresRealParams
postgresParamsParser =
    #connString .:: connParser <*<
    #connNum    .:: connNumParser <*<
    #maxPending .:: maxPendingParser
  where
    connParser = strOption $
        long "sql-conn-str" <>
        metavar "TEXT" <>
        help "A libpq connection string. Behavior would be exactly as specified for this \
             \method: https://hackage.haskell.org/package/postgresql-simple-0.6/docs/Database-PostgreSQL-Simple.html#v:connectPostgreSQL"
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

educatorBotConfigParser :: OptModParser EducatorBotConfig
educatorBotConfigParser = #params .:+
    (#paramsType .:: (enabledP <|> disabledP) <*<
     #enabled .:-
        (#seed            .:: seedParser <*<
         #operationsDelay .:: delayParser
        )
    )
  where
    enabledP = flag' "enabled" $
        long "educator-bot" <>
        help "Enable bot which would automatically react on student actions. \
              \To disable it provide the flag `--educator-no-bot` instead."
    disabledP = flag' "disabled" $
        long "educator-no-bot" <>
        help "Disable bot which would automatically react on student actions. \
              \To enable it provide the flag `--educator-bot` instead."
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
    #serverParams .:< serverParamsParser "Educator" <*<
    #botConfig .:< educatorBotConfigParser <*<
    #educatorAPINoAuth .:: educatorApiNoAuthParser <*<
    #studentAPINoAuth .:: studentApiNoAuthParser

pdfResourcesPathParser :: Parser FilePath
pdfResourcesPathParser = strOption $
    long "pdf-resource-path" <>
    metavar "FILEPATH" <>
    help "Path to PDF templates. When relative path is specified, \
         \application directory is considered its root."

pdfIssuerNameParser :: Parser ItemDesc
pdfIssuerNameParser = option (eitherReader $ left toString . toItemDesc . fromString) $
    long "cert-issuer-name" <>
    metavar "TEXT" <>
    help "Name of the certificate issuer"

pdfIssuerUrlParser :: Parser ItemDesc
pdfIssuerUrlParser = option (eitherReader $ left toString . toItemDesc . fromString) $
    long "cert-issuer-url" <>
    metavar "TEXT" <>
    help "Url of the certificate issuer"

pubAddressParser :: Parser PubAddress
pubAddressParser = option (eitherReader $ pubAddrFromText . fromString) $
    long "pub-address" <>
    metavar "ADDR" <>
    help "Solidity address (ETH/BSC) of the Educator"

educatorConfigParser :: OptModParser EducatorConfig
educatorConfigParser =
    #educator .:<
        (#db .:< postgresParamsParser <*<
         #appDir .:< appDirParamParser <*<
         #pubAddress .:: pubAddressParser <*<
         #api .:< educatorWebConfigParser <*<
         #publishing .:<
            (#period .:: publishingPeriodParser) <*<
         #certificates .:<
            (#resources .:: pdfResourcesPathParser <*<
             #issuer .:<
               (#name .:: pdfIssuerNameParser <*<
                #website .:: pdfIssuerUrlParser
               )
            )
        )

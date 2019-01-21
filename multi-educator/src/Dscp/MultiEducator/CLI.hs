{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLabels #-}

-- | CLI for educator.

module Dscp.MultiEducator.CLI
    ( multiEducatorKeyParamsParser
    , multiEducatorConfigParser
    ) where

import Loot.Config (OptModParser, uplift, (.::), (.:<), (<*<))
import Options.Applicative (Parser, help, long, metavar, strOption)

import Dscp.Educator.CLI (educatorWebConfigParser, postgresParamsParser, publishingPeriodParser)
import Dscp.MultiEducator.Config (MultiEducatorConfig)
import Dscp.MultiEducator.Launcher.Params (MultiEducatorKeyParams (..))
import Dscp.Witness.CLI (witnessConfigParser)

multiEducatorKeyParamsParser :: Parser MultiEducatorKeyParams
multiEducatorKeyParamsParser = MultiEducatorKeyParams <$>
    strOption (long "educator-key-dir" <>
               metavar "PATH" <>
               help "Path to the directory with educator keys")

multiEducatorConfigParser :: OptModParser MultiEducatorConfig
multiEducatorConfigParser =
    uplift witnessConfigParser <*<
    #educator .:<
        (#db .:< postgresParamsParser <*<
         #keys .:: multiEducatorKeyParamsParser <*<
         #api .:< educatorWebConfigParser <*<
         #publishing .:<
             (#period .:: publishingPeriodParser
             )
        )

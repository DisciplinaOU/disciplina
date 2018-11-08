{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLabels #-}

-- | CLI for educator.

module Dscp.MultiEducator.CLI
    ( multiEducatorKeyParamsParser
    , multiEducatorConfigParser
    ) where

import Loot.Config (OptParser, upcast, (.::), (.:<), (.<>))
import Options.Applicative (Parser, help, long, metavar, strOption)

import Dscp.Educator.CLI (educatorWebParamsParser, publishingPeriodParser, sqliteParamsParser)
import Dscp.MultiEducator.Config (MultiEducatorConfig)
import Dscp.MultiEducator.Launcher.Params (MultiEducatorKeyParams (..))
import Dscp.Witness.CLI (witnessConfigParser)

multiEducatorKeyParamsParser :: Parser MultiEducatorKeyParams
multiEducatorKeyParamsParser = MultiEducatorKeyParams <$>
    strOption (long "educator-key-dir" <>
               metavar "PATH" <>
               help "Path to the directory with educator keys")

multiEducatorConfigParser :: OptParser MultiEducatorConfig
multiEducatorConfigParser =
    fmap upcast witnessConfigParser .<>
    #educator .:<
        (#db .:: sqliteParamsParser .<>
         #keys .:: multiEducatorKeyParamsParser .<>
         #api .:: educatorWebParamsParser .<>
         #publishing .:<
             (#period .:: publishingPeriodParser
             )
        )

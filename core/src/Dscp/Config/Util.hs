{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Making config with lootbox.

module Dscp.Config.Util
    ( ConfigBuildError (..)
    , buildConfig

    , configPathParser
    ) where

import qualified Data.Text.Buildable
import Data.Yaml (ParseException, decodeFileEither)
import Fmt (blockListF)
import Loot.Config (ConfigKind (Final, Partial), ConfigRec, finalise)
import qualified Options.Applicative as Opt
import qualified Text.Show

import Dscp.Util (leftToThrow)

data ConfigBuildError
    = ConfigReadError !ParseException
      -- ^ Parse + I/O errors
    | ConfigIncomplete ![String]
      -- ^ Some values are not defined in resulting config

instance Show ConfigBuildError where
    show = toString . pretty

instance Buildable ConfigBuildError where
    build = \case
        ConfigReadError err -> "Config parse failed: " <> show err
        ConfigIncomplete missing -> "Missing entries: " <> blockListF missing

instance Exception ConfigBuildError

-- | Reads config file and fills missing values with ones in given default
-- config.
-- Function has complex constraint you don't need to bother with, it will be
-- satisfied if you make up config type properly.
-- TODO: consider CLI params as well
buildConfig
    :: (MonadIO m, MonadThrow m, _)
    => ConfigRec 'Partial o -> FilePath -> m (ConfigRec 'Final o)
buildConfig defConfig configPath = liftIO $ do
    fileConfig <- decodeFileEither configPath >>= leftToThrow ConfigReadError
    config <-
        leftToThrow ConfigIncomplete $
        finalise (defConfig <> fileConfig)
    return config

-- | Get path to config file.
configPathParser :: Opt.Parser FilePath
configPathParser = Opt.strOption $
    Opt.short 'c' <>
    Opt.long "config" <>
    Opt.metavar "FILEPATH" <>
    Opt.help "Path to configuration file"

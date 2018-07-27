{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Making config with lootbox.

module Dscp.Config.Util
    (
      type (+++)

    , ConfigBuildError (..)
    , buildConfig

    , configPathParser

    , HasGiven
    , giveL
    , HasGivenC
    , giveLC
    ) where

import Data.Reflection (Given (..))
import qualified Data.Text.Buildable
import Data.Yaml (ParseException, decodeFileEither)
import Fmt (blockListF)
import Loot.Base.HasLens (HasLens', lensOf)
import Loot.Config (ConfigKind (Final, Partial), ConfigRec, HasLensC, finalise, lensOfC)
import qualified Options.Applicative as Opt
import qualified Text.Show

import Dscp.Util (leftToThrow)


----------------------------------------------------------------------------
-- Appending configs (as lists)
----------------------------------------------------------------------------

-- Because there is no publicly availble type family for list concatenation.
type family (+++) (as :: [k]) (bs :: [k]) :: [k] where
    (+++) a '[] = a
    (+++) '[] b = b
    (+++) (a ': as) bs = a ': (as +++ bs)

----------------------------------------------------------------------------
-- Building config
----------------------------------------------------------------------------

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
buildConfig ::
       (MonadIO m, MonadThrow m, _)
    => FilePath
    -> (ConfigRec 'Partial o -> IO (ConfigRec 'Partial o))
    -> m (ConfigRec 'Final o)
buildConfig configPath filler = liftIO $ do
    fileConfig <- decodeFileEither configPath >>= leftToThrow ConfigReadError
    fileConfigFilled <- filler fileConfig
    config <-
        leftToThrow ConfigIncomplete $
        finalise fileConfigFilled
    return config

-- | Get path to config file.
configPathParser :: Opt.Parser FilePath
configPathParser = Opt.strOption $
    Opt.short 'c' <>
    Opt.long "config" <>
    Opt.metavar "FILEPATH" <>
    Opt.help "Path to configuration file"

----------------------------------------------------------------------------
-- Accessing config with lens
----------------------------------------------------------------------------

type HasGiven is v =
    (Given (ConfigRec 'Final is), HasLens' (ConfigRec 'Final is) v)

giveL :: forall is v . HasGiven is v => v
giveL = given @(ConfigRec 'Final is) ^. (lensOf @v)

type HasGivenC path is v =
    (Given (ConfigRec 'Final is), HasLensC path is v)

giveLC :: forall path is v . HasGivenC path is v => v
giveLC = given ^. (lensOfC @path @is @v)

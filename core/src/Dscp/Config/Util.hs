{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Making config with lootbox.

module Dscp.Config.Util
    (
      type (+++)
    , type (<:)
    , rcast
    , rreplace

    , ConfigParams (..)
    , configParamsParser
    , ConfigBuildError (..)
    , buildConfig
    , fillExpandedConfig

    , HasGiven
    , giveL
    , HasGivenC
    , giveLC
    ) where

import Data.Vinyl.Lens (rcast, rreplace, type (<:))
import Data.Reflection (reifySymbol)
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
import Data.Reflection (Given (..))
import qualified Data.Text.Buildable
import Data.Yaml (ParseException, decodeFileEither, FromJSON(..), withObject, (.:?))
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

-- | Configuration parameters.
data ConfigParams = ConfigParams
    { cpPath :: FilePath
      -- ^ Pass to the configuration file.
    , cpConfigKey :: String
      -- ^ Configuration key.
    } deriving (Show)

newtype ConfigWithKey (s :: Symbol) o = ConfigWithKey o

instance (KnownSymbol s, FromJSON o) => FromJSON (ConfigWithKey s o) where
    parseJSON =
         withObject "Configuration object with keys-confignames" $ \o -> do
             let s' = symbolVal (Proxy :: Proxy s)
             configM <- o .:? fromString s'
             let failNothing = fail $ "Configuration key not present: " <> s'
             config <- maybe failNothing pure configM
             ConfigWithKey <$> parseJSON config

-- | Reads config file and fills missing values with ones in given default
-- config.
-- Function has complex constraint you don't need to bother with, it will be
-- satisfied if you make up config type properly.
-- TODO: consider CLI params as well
buildConfig ::
       (MonadIO m, MonadThrow m, _)
    => ConfigParams
    -> (ConfigRec 'Partial o -> IO (ConfigRec 'Partial o))
    -> m (ConfigRec 'Final o)
buildConfig ConfigParams{..} filler =
    reifySymbol cpConfigKey $ \(_ :: Proxy s) -> liftIO $ do
        ((ConfigWithKey fileConfig) :: ConfigWithKey s (ConfigRec 'Partial o)) <-
            decodeFileEither cpPath >>= leftToThrow ConfigReadError
        fileConfigFilled <- filler fileConfig
        config <-
            leftToThrow ConfigIncomplete $
            finalise fileConfigFilled
        pure config

-- | Utility function for filling up a config reusing existing function
-- for a subconfig.
fillExpandedConfig ::
       forall xs ys . (xs <: ys)
    => (ConfigRec 'Partial xs -> IO (ConfigRec 'Partial xs))
    -> ConfigRec 'Partial ys
    -> IO (ConfigRec 'Partial ys)
fillExpandedConfig filler cfg = flip rreplace cfg <$> filler (rcast cfg)

-- | CLI parser for config parameters.
configParamsParser :: Opt.Parser ConfigParams
configParamsParser = do
    cpPath <- confPathParser
    cpConfigKey <- confKeyParser
    pure ConfigParams {..}
  where
    confPathParser = Opt.strOption $
        Opt.short 'c' <>
        Opt.long "config" <>
        Opt.metavar "FILEPATH" <>
        Opt.help "Path to configuration file."
    confKeyParser = Opt.strOption $
        Opt.long "config-key" <>
        Opt.metavar "STRING" <>
        Opt.help "Configuration key. Name of top-level section of \
                 \configuration to use."

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

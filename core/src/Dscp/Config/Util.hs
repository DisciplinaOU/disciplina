{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Making config with lootbox.

module Dscp.Config.Util
    (
      -- * Re-exports from `vinyl`
      type (++)
    , type (<:)
    , rcast
    , rreplace

      -- * Re-exports from 'Loot.Config'
    , sub
    , option
    , tree
    , branch
    , selection
    , finaliseDeferredUnsafe

      -- * Config parsing and building
    , ConfigParams (..)
    , configParamsParser
    , ConfigBuildError (..)
    , buildConfig
    , fillExpandedConfig

      -- * Utility config types and functions
    , ConfigMaybe
    , ConfigMaybeRec
    , ConfigMaybeRecP
    , whenConfigJust
    , selectBranchParser

      -- * Helper lenses/classes
    , HasGiven
    , giveL
    , HasGivenC
    , giveLC

      -- * Helper functions
    , peekBranch
    ) where

import Control.Applicative.Combinators.NonEmpty as NonEmpty (some)
import Control.Lens (Contravariant, to)
import Data.Aeson (Result (..), Value (Object), fromJSON)
import qualified Data.HashMap.Strict as HM
import Data.Reflection (reifySymbol)
import Data.Reflection (Given (..))
import qualified Data.Text.Buildable
import Data.Vinyl.Derived (Label)
import Data.Vinyl.Lens (type (<:), rcast, rreplace)
import Data.Vinyl.TypeLevel (type (++))
import Data.Yaml (FromJSON (..), ParseException (AesonException), decodeFileEither, withObject,
                  (.:?))
import Fmt (blockListF)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Loot.Base.HasLens (HasLens', lensOf)
import Loot.Config ((::+), (::-), (.::), (<*<), OptModParser, ConfigKind (Final, Partial),
                    ConfigRec, HasLensC, finalise, finaliseDeferredUnsafe,
                    lensOfC, option, sub, tree, branch, selection)
import Loot.Config.Record (HasBranch, HasOption)
import qualified Options.Applicative as Opt
import qualified Text.Show

import Dscp.Util (leftToThrow)

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
    { cpPaths     :: NonEmpty FilePath
      -- ^ Paths to the configuration files.
    , cpConfigKey :: String
      -- ^ Configuration key.
    } deriving (Show)

newtype ConfigWithKey (s :: Symbol) o = ConfigWithKey o
    deriving (Semigroup)

instance (KnownSymbol s, FromJSON o) => FromJSON (ConfigWithKey s o) where
    parseJSON =
         withObject "Configuration object with keys-confignames" $ \o -> do
             let s' = symbolVal (Proxy :: Proxy s)
             configM <- o .:? fromString s'
             let failNothing = fail $ "Configuration key not present: " <> s'
             config <- maybe failNothing pure configM
             ConfigWithKey <$> parseJSON config

-- | Reads config files and fills missing values with ones in given default
-- config. If multiple config files are provided, they are override-merged.
-- Merging only works for vinyl records in config though.
-- Function has complex constraint you don't need to bother with, it will be
-- satisfied if you make up config type properly.
buildConfig ::
       (MonadIO m, MonadThrow m, _)
    => ConfigParams
    -> (ConfigRec 'Partial o -> IO (ConfigRec 'Partial o))
    -> m (ConfigRec 'Final o)
buildConfig ConfigParams{..} filler =
    reifySymbol cpConfigKey $ \(_ :: Proxy s) -> liftIO $ do
        rawConfig :: Value <- fmap mergeOverrideAll . forM cpPaths $
            decodeFileEither >=> leftToThrow ConfigReadError
        ((ConfigWithKey fileConfig) :: ConfigWithKey s (ConfigRec 'Partial o)) <-
            leftToThrow (ConfigReadError . AesonException) .
            resToEither $ fromJSON rawConfig
        fileConfigFilled <- filler fileConfig
        config <-
            leftToThrow ConfigIncomplete $
            finalise fileConfigFilled
        pure config

-- | Aeson's 'Result' to 'Either'.
resToEither :: Result a -> Either String a
resToEither (Error s)   = Left s
resToEither (Success a) = Right a

-- | Merges two 'Value's, keys in right one recursively
-- override keys in left.
mergeOverride :: Value -> Value -> Value
mergeOverride (Object o1) (Object o2) =
    Object $ HM.unionWith mergeOverride o1 o2
mergeOverride _ b = b

-- | Merges a container of 'Value's, left to right.
mergeOverrideAll :: (Container c, Element c ~ Value) => c -> Value
mergeOverrideAll = foldl' mergeOverride (Object mempty)

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
    cpPaths <- NonEmpty.some confPathParser
    cpConfigKey <- confKeyParser
    pure ConfigParams {..}
  where
    confPathParser = Opt.strOption $
        Opt.short 'c' <>
        Opt.long "config" <>
        Opt.metavar "FILEPATH" <>
        Opt.help "Path to configuration file. Multiple -c options can \
                 \be provided, in which case configuration is merged. \
                 \The order matters, the latter one overrides the former."
    confKeyParser = Opt.strOption $
        Opt.long "config-key" <>
        Opt.metavar "STRING" <>
        Opt.help "Configuration key. Name of top-level section of \
                 \configuration to use."

----------------------------------------------------------------------------
-- Utility config types and functions
----------------------------------------------------------------------------

-- | Utility config type equivalent to 'Maybe'
-- Note: the reason why this (and other) config tree types are defined including
-- the tree itself and not just the group of branches it contains is that if we
-- were to do that, the inner config type would not have access to the tree
-- 'selection' and thus hardly be of any value by itself.
type ConfigMaybe cfg =
   '[ "maybe" ::+
       '[ "nothing" ::- '[]
        , "just"    ::- cfg
        ]
    ]

type ConfigMaybeRec cfg = ConfigRec 'Final (ConfigMaybe cfg)
type ConfigMaybeRecP cfg = ConfigRec 'Partial (ConfigMaybe cfg)

whenConfigJust
    :: Applicative m
    => ConfigMaybeRec cfg -> (ConfigRec 'Final cfg -> m ()) -> m ()
whenConfigJust configMaybe f = case configMaybe ^. tree #maybe . selection of
    "nothing" -> pure ()
    "just"    -> f $ configMaybe ^. tree #maybe . peekBranch #just
    sel       -> error $ "unknown ConfigMaybe type: " <> fromString sel

-- | Utility function for a common 'tree' content parsing pattern.
-- When a tree has two branches, one of witch is empty, it is desirable to have
-- a flag parser to select the empty branch and a parser for the non-empty branch
-- content that also selects this branch (instead of having one more flag).
-- Note that this function is made to allow this while avoiding to set a default
-- selection, respecting the 'mempty' property that we check for in the tests.
selectBranchParser
    :: HasOption l is v
    => Label l          -- ^ Label of the tree selector
    -> Opt.Parser v     -- ^ Parser (usually flag) to select the empty branch
    -> v                -- ^ Name of the non-empty branch
    -> OptModParser is  -- ^ Parser for the non-empty branch
    -> OptModParser is
selectBranchParser treeType emptyBranchFlag fullBranchName fullBranchParser =
    (treeType .:: emptyBranchFlag) <|>
    (treeType .:: pure fullBranchName <*< fullBranchParser)

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

-- | Helper function to read the content of a 'finalise'd 'branch' by unsafely
-- traversing the Maybe. This should only be used on the selected 'branch', in
-- which case no error will be thrown.
peekBranch
    :: (HasBranch l is us, Contravariant g, Functor g, a ~ ConfigRec 'Final us)
    => Label l
    -> (a -> g a)
    -> ConfigRec 'Final is
    -> g (ConfigRec 'Final is)
peekBranch l = branch l . to (fromMaybe (error "accessed unselected branch"))

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Base config for any node.

module Dscp.Config.Base
    ( BaseConfig (..)
    , HasBaseConfig
    , baseConfig
    , withBaseConfig
    , buildBaseConfig
    ) where

import Control.Lens (zoom, (?=))
import Data.Default (Default (..))
import Data.Reflection (Given (..), give)
import Loot.Config ((:::), (::<), ConfigKind (Partial), ConfigRec, option, sub)

import Dscp.Config.AppDir (AppDirectoryParam (..), prepareAppDir)
import Dscp.Config.Util (buildConfig)

---------------------------------------------------------------------
-- Reading config
---------------------------------------------------------------------

type BaseOptions =
   '[ "home" ::: AppDirectoryParam
    , "memes" ::<  -- just as use case
       '[

        ]
    ]

type BaseConfigRec = ConfigRec 'Partial BaseOptions

defBaseConfig :: BaseConfigRec
defBaseConfig = flip execState def $ do
    option #home ?= AppDirectoryOS
    zoom (sub #memes) $ do
        pass

---------------------------------------------------------------------
-- Config itself
---------------------------------------------------------------------

data BaseConfig = BaseConfig
    { bcAppDirectory :: !FilePath

    }

type HasBaseConfig = Given BaseConfig

baseConfig :: HasBaseConfig => BaseConfig
baseConfig = given

withBaseConfig :: BaseConfig -> (HasBaseConfig => a) -> a
withBaseConfig = give

-- | Read and process config file, producing ready pack of params for carring
-- it in main code.
buildBaseConfig :: (MonadIO m, MonadThrow m) => FilePath -> m BaseConfig
buildBaseConfig configPath = do
    config <- buildConfig defBaseConfig configPath

    let appDirParam = config ^. option #home
    bcAppDirectory <- prepareAppDir appDirParam

    return BaseConfig{..}

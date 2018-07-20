-- | All educator's configurations.

module Dscp.Educator.Config
    ( EducatorConfig
    , HasEducatorConfig
    , withEducatorConfig
    ) where

import Dscp.Config (BaseConfig, HasBaseConfig, withBaseConfig)

type EducatorConfig = BaseConfig

type HasEducatorConfig = HasBaseConfig

withEducatorConfig :: EducatorConfig -> (HasEducatorConfig => a) -> a
withEducatorConfig = withBaseConfig

module Dscp.MultiEducator.Launcher.Params
       ( MultiEducatorKeyParams (..)
       ) where

import Data.Aeson (FromJSON (..))

-- | Educator key parameters.
newtype MultiEducatorKeyParams = MultiEducatorKeyParams
    { unMultiEducatorKeyParams :: FilePath
      -- ^ Educator key folder
    } deriving (Eq, Show, FromJSON)

module Dscp.Educator.Error
       ( EducatorPublishError (..)
       ) where

import Dscp.Core
import Fmt (build, (+|), (|+))
import qualified Text.Show

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data EducatorPublishError
    = PrivateAndPublicChainsDiverged PrivateHeaderHash

instance Show EducatorPublishError where
    show = toString . pretty

instance Buildable EducatorPublishError where
    build (PrivateAndPublicChainsDiverged h) =
        "Given publication " +| h |+ " does not seem to belong to the educator"

instance Exception EducatorPublishError

module Dscp.Educator.Error
       ( EducatorPublishError (..)
       ) where

import Universum
import Dscp.Core
import Fmt (Buildable (..), (+|), (|+), pretty)
import qualified Text.Show

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data EducatorPublishError
    = PrivateAndPublicChainsDiverged PrivateHeaderHash

instance Show EducatorPublishError where
    show = toString @Text . pretty

instance Buildable EducatorPublishError where
    build (PrivateAndPublicChainsDiverged h) =
        "Given publication " +| h |+ " does not seem to belong to the educator"

instance Exception EducatorPublishError

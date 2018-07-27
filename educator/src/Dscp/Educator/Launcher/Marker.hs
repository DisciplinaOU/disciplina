module Dscp.Educator.Launcher.Marker
    ( EducatorNode
    ) where

import qualified Data.Text.Buildable

import Dscp.Educator.Config
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.Resource.Keys

-- | Indicator of educator.
data EducatorNode

-- Used not only in logs, change with care (@martoon).
instance Buildable (Proxy EducatorNode) where
    build _ = "educator"

instance HasWitnessConfig =>
         AllocResource EducatorKeyParams (KeyResources EducatorNode) where
    allocResource (EducatorKeyParams baseParams) =
        buildComponentR "educator keys"
            (linkStore baseParams Nothing)
            (\_ -> pass)

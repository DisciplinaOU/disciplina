module Dscp.Educator.Launcher.Marker
    ( EducatorNode
    ) where

import qualified Data.Text.Buildable

-- | Indicator of educator.
data EducatorNode

instance Buildable (Proxy EducatorNode) where
    build _ = "educator node"

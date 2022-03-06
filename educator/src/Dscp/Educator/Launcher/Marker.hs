module Dscp.Educator.Launcher.Marker
    ( EducatorNode
    ) where

import Universum
import Fmt (Buildable (..))

-- | Indicator of educator.
data EducatorNode

-- Used not only in logs, change with care (@martoon).
instance Buildable (Proxy EducatorNode) where
    build _ = "educator"

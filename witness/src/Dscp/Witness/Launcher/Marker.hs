module Dscp.Witness.Launcher.Marker
    ( WitnessNode
    ) where

import qualified Data.Text.Buildable

-- | Indicator of educator.
data WitnessNode

-- Used not only in logs, change with care (@martoon).
instance Buildable (Proxy WitnessNode) where
    build _ = "witness"

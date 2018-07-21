module Dscp.Witness.Launcher.Marker
    ( WitnessNode
    ) where

import qualified Data.Text.Buildable

-- | Indicator of witness.
data WitnessNode

instance Buildable (Proxy WitnessNode) where
    build _ = "witness node"

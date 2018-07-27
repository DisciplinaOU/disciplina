module Dscp.Witness.Launcher.Marker
    ( WitnessNode
    ) where

import qualified Data.Text.Buildable

import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.Resource.Keys
import Dscp.Witness.Config

-- | Indicator of witness.
data WitnessNode

-- Used not only in logs, change with care (@martoon).
instance Buildable (Proxy WitnessNode) where
    build _ = "witness"

instance HasWitnessConfig =>
         AllocResource WitnessKeyParams (KeyResources WitnessNode) where
    allocResource WitnessKeyParams{..} =
        buildComponentR "witness keys"
            (linkStore wkpBase wkpCommittee)
            (\_ -> pass)

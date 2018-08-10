-- | SD execution parameters.

module Dscp.Snowdrop.IOCtx
    ( IOCtx
    ) where

import qualified Snowdrop.Model.Execution as SD

import Dscp.Snowdrop.Configuration

-- | Alias for ERoComp with concrete config types.
type IOCtx chgAccum = SD.IOCtx chgAccum Ids Values

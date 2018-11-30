-- | Block creation.

module Dscp.Witness.Logic.Creation
    ( createPayload
    , createBlock
    ) where

import Dscp.Core
import Dscp.Crypto
import Dscp.Resource.Keys
import Dscp.Snowdrop
import Dscp.Witness.Launcher.Context
import Dscp.Witness.Logic.Getters
import Dscp.Witness.Mempool (takeTxsMempool)
import Dscp.Witness.SDLock

-- | Empty mempool(s), create block body.
createPayload
    :: forall ctx m
    .  (WitnessWorkMode ctx m, WithinWriteSDLock)
    => m BlockBody
createPayload = BlockBody <$> takeTxsMempool @ctx

-- | Create a public block.
createBlock
    :: (WitnessWorkMode ctx m, WithinWriteSDLock)
    => SlotId
    -> m Block
createBlock newSlot = do
    tipHeader <- runSdM getTipHeader
    let tipHash = headerHash tipHeader
    let diff = hDifficulty tipHeader + 1

    payload <- createPayload
    sk <- ourSecretKeyData @WitnessNode
    let sgn = sign (skSecret sk) $ BlockToSign diff newSlot tipHash (hash payload)
    let header = Header sgn (skPublic sk) diff newSlot tipHash
    let block = Block header payload
    pure block

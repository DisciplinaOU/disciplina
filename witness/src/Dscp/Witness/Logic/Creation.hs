-- | Block creation.

module Dscp.Witness.Logic.Creation
    ( createPayload
    , createBlock
    ) where

import Dscp.Core
import Dscp.Crypto
import Dscp.Resource.Keys (ourSecretKey)
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
    => (forall a . SdM a -> m a)
    -> SlotId
    -> m Block
createBlock runner newSlot = do
    tipHeader <- runner getTipHeader
    let tipHash = headerHash tipHeader
    let diff = hDifficulty tipHeader + 1

    payload <- createPayload
    sk <- ourSecretKey @WitnessNode
    let sgn = sign sk $ BlockToSign diff newSlot tipHash (hash payload)
    let header = Header sgn (toPublic sk) diff newSlot tipHash
    let block = Block header payload
    pure block

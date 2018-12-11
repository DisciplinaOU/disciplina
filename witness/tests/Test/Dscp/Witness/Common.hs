module Test.Dscp.Witness.Common
    ( selectGenesisSecret
    , genSafeOutAddr
    , genSafeTxOuts
    , dumpBlock
    ) where

import Loot.Base.HasLens (glensOf)

import Dscp.Core
import Dscp.Crypto
import Dscp.Resource.Keys
import Dscp.Util.Test
import Dscp.Witness

-- | Arbitrarly choose genesis secret.
selectGenesisSecret :: Gen SecretKey
selectGenesisSecret = elements testGenesisSecrets

-- | Generate output `Address` which does not make transaction invalid.
genSafeOutAddr :: Gen Address
genSafeOutAddr = arbitrary `suchThat` (`notElem` testGenesisAddresses)

-- | Generate output `TxOut` which does not make transaction invalid.
genSafeTxOuts :: Word64 -> Gen Word32 -> Gen [TxOut]
genSafeTxOuts maxVal genN = do
    n <- fromIntegral <$> genN
    txOutAddrs <- vectorUniqueOf n genSafeOutAddr
    txOutValues <- vectorOf n $ Coin <$> choose (1, maxVal)
    return $ zipWith TxOut txOutAddrs txOutValues

-- | Dump all mempool transactions into a new block.
dumpBlock
    :: (TestWitnessWorkMode ctx m, WithinWriteSDLock)
    => m HeaderHash
dumpBlock = do
    slotId <- rewindToNextSlot
    let issuerKey = KeyResources . mkSecretKeyData $ testFindSlotOwner slotId

    local (glensOf @WitnessNode .~ issuerKey) $ do
        block <- createBlock slotId
        void $ applyBlock block
        return (headerHash block)

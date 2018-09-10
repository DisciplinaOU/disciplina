module Test.Dscp.Witness.BlockSpec where

import Control.Lens (ix, to)
import qualified Data.List as L
import qualified GHC.Exts as Exts
import Test.QuickCheck.Modifiers (getPositive)
import Test.QuickCheck.Monadic (pre)

import Dscp.Core
import Dscp.Crypto
import Dscp.Resource.Keys
import Dscp.Util.Test
import Dscp.Witness
import Test.Dscp.Witness.Mode

-- | Submit block to validation.
submitBlock
    :: (HasWitnessConfig, WithinWriteSDLock)
    => Block -> WitnessTestMode ()
submitBlock = void . applyBlock

-- | Make sound list of blocks.
makeBlocksChain :: HasWitnessConfig => Int -> SecretKey -> NonEmpty Block
makeBlocksChain n issuer
    | n <= 0 = error "makeBlocksChain: n <= o"
    | otherwise =
        Exts.fromList $
        fix $ \blocks ->
        zip3 [1..n] (genesisBlock : blocks) issuingSlots
        <&> \(i, prevBlock, hSlotId) ->
            let hDifficulty = fromIntegral i
                hPrevHash = hash (bHeader prevBlock)
                bBody = BlockBody []
                toSign = BlockToSign hDifficulty hSlotId hPrevHash (hash bBody)
                hSignature = sign issuer toSign
            in Block
            { bBody
            , bHeader = Header
              { hIssuer = toPublic issuer
              , hDifficulty, hSlotId, hPrevHash, hSignature
              }
            }
  where
    issuerAddr = mkAddr $ toPublic issuer
    issuingSlots = filter (committeeOwnsSlot testCommittee issuerAddr) [1..]

resignBlock :: SecretKey -> Block -> Block
resignBlock issuer block@Block{..} =
    let Header{..} = bHeader
        toSign = BlockToSign hDifficulty hSlotId hPrevHash (hash bBody)
        sig = sign issuer toSign
    in  block & bHeaderL . hSignatureL .~ sig

-- | Given correct chain, and spoiling function, modifies one of blocks
-- in a chain and checks that its application fails.
submitSpoiledChain
    :: (HasWitnessConfig, WithinWriteSDLock)
    => NonEmpty Block
    -> (Block -> Gen Block)
    -> PropertyM WitnessTestMode Bool
submitSpoiledChain chain spoilBlock = do
    let lastBlock = last chain
    badLastBlock <- pick $ spoilBlock lastBlock
    pre (badLastBlock /= lastBlock)
    lift $ do
        mapM_ submitBlock (init chain)
        throwsSome $ submitBlock badLastBlock

spec :: Spec
spec = describe "Block validation + application" $ do
  describe "Block header" $ do
    it "Good block is fine" $ witnessProperty $ do
        issuer <- lift $ getSecretKey @WitnessNode
        let blocks = makeBlocksChain 1 issuer
        lift . noThrow $ mapM_ submitBlock blocks

    it "Several good blocks are is fine" $ witnessProperty $ do
        issuer <- lift $ getSecretKey @WitnessNode
        n <- pick $ getPositive <$> arbitrary
        let blocks = makeBlocksChain n issuer
        lift . noThrow $ mapM_ submitBlock blocks

    it "Wrong previous block hash is not fine" $ witnessProperty $ do
        issuer <- lift $ getSecretKey @WitnessNode
        n <- pick $ getPositive <$> arbitrary
        let blocks = makeBlocksChain n issuer
        let spoilBlock block = do
                badPrevHash <- arbitrary
                return $ block & bHeaderL . hPrevHashL .~ badPrevHash
        submitSpoiledChain blocks spoilBlock

    it "Applying block at wrong slot is not fine" $ witnessProperty $ do
        issuer <- lift $ getSecretKey @WitnessNode
        let issuerAddr = mkAddr $ toPublic issuer
            block :| [] = makeBlocksChain 1 issuer
            ownedSlot = committeeOwnsSlot testCommittee issuerAddr
            badSlot = L.head $ filter (not . ownedSlot) [1..]
            badBlock = block & bHeaderL . hSlotIdL .~ badSlot
                             & resignBlock issuer
        lift . throwsSome $ submitBlock badBlock

    it "Wrong difficulty is fatal" $ witnessProperty $ do
        issuer <- lift $ getSecretKey @WitnessNode
        n <- pick $ getPositive <$> arbitrary
        let blocks = makeBlocksChain n issuer
        let spoilBlock block = do
                badDiff <- arbitrary
                return $ block & bHeaderL . hDifficultyL .~ badDiff
                               & resignBlock issuer
        submitSpoiledChain blocks spoilBlock

    it "Block slot only increases over time" $ witnessProperty $ do
        issuer <- lift $ getSecretKey @WitnessNode
        n <- pick $ (+1) . getPositive <$> arbitrary
        -- going to modify block before the last one with too high slotId
        let issuerAddr = mkAddr $ toPublic issuer
            blocks = makeBlocksChain n issuer
            ownedSlot = committeeOwnsSlot testCommittee issuerAddr
            lastSlot = blocks ^. to last . bHeaderL . hSlotIdL
            futureSlot = L.head $ filter (not . ownedSlot) [99999..]
        oddSlot <- pick $ elements [lastSlot, futureSlot]
        let badBlocks = blocks & ix (n - 2) %~
                           \block -> block & bHeaderL . hSlotIdL .~ oddSlot
                                           & resignBlock issuer
        lift $ do
            mapM_ submitBlock (init badBlocks)
            throwsSome $ submitBlock (last badBlocks)

    it "Wrong signature is not fine" $ witnessProperty $ do
        issuer <- lift $ getSecretKey @WitnessNode
        let block1 :| [] = makeBlocksChain 1 issuer
        block2 <- pick arbitrary
        mixBlock <- pick $ arbitraryUniqueMixture block1 block2
        lift . throwsSome $ submitBlock mixBlock

  describe "Block content" $ do
    it "Content is verified" $ witnessProperty $ do
        issuer <- lift $ getSecretKey @WitnessNode
        let block :| [] = makeBlocksChain 1 issuer
        tx <- pick arbitrary
        let badTx = tx & twTxL . txInAccL . tiaNonceL .~ 999
        let badBlock = block & bBodyL . bbTxsL .~ one (GMoneyTxWitnessed badTx)
        lift . throwsSome $ submitBlock badBlock

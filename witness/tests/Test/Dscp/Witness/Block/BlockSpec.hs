module Test.Dscp.Witness.Block.BlockSpec where

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

-- | Find who should sign block at given slot.
findSlotOwner :: SlotId -> SecretKey
findSlotOwner slot =
    fromMaybe (error "Failed to find slot owner") $
    find (\sk -> committeeOwnsSlot testCommittee (mkAddr $ toPublic sk) slot)
        testCommitteeSecrets

-- | Update block signature to catch up change of content.
resignBlockAs :: SecretKey -> Block -> Block
resignBlockAs issuer block@Block{..} =
    let Header{..} = bHeader
        toSign = BlockToSign hDifficulty hSlotId hPrevHash (hash bBody)
        sig = sign issuer toSign
    in  block & bHeaderL . hSignatureL .~ sig

-- | Update block signature to catch up change of content.
resignBlock :: Block -> Block
resignBlock block =
    resignBlockAs (findSlotOwner $ hSlotId $ bHeader block) block

-- | Submit block to validation.
submitBlock
    :: (HasWitnessConfig, WithinWriteSDLock)
    => Block -> WitnessTestMode ()
submitBlock = void . applyBlock

-- | Make sound list of blocks.
-- Given secret key is assumed to correspond to educator - author of blocks -
-- and witness who publishes this block.
makeBlocksChain :: HasWitnessConfig => Int -> NonEmpty Block
makeBlocksChain n
    | n <= 0 = error "makeBlocksChain: n <= 0"
    | otherwise =
        Exts.fromList $
        fix $ \blocks ->
        zip [1..n] (genesisBlock : blocks)
        <&> \(i, prevBlock) ->
            let hSlotId = fromIntegral i
                hDifficulty = fromIntegral i
                issuer = findSlotOwner hSlotId
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
        let blocks = makeBlocksChain 1
        lift . noThrow $ mapM_ submitBlock blocks

    it "Several good blocks are is fine" $ witnessProperty $ do
        n <- pick $ getPositive <$> arbitrary
        let blocks = makeBlocksChain n
        lift . noThrow $ mapM_ submitBlock blocks

    it "Wrong previous block hash is not fine" $ witnessProperty $ do
        n <- pick $ getPositive <$> arbitrary
        let blocks = makeBlocksChain n
        let spoilBlock block = do
                badPrevHash <- arbitrary
                return $ block & bHeaderL . hPrevHashL .~ badPrevHash
                               & resignBlock
        submitSpoiledChain blocks spoilBlock

    it "Applying block not by committee member is not fine" $ witnessProperty $ do
        issuer <- pick arbitrary
        let block :| [] = makeBlocksChain 1
            badBlock = block & resignBlockAs issuer
        pre (badBlock /= block)
        lift . throwsSome $ submitBlock badBlock

    it "Wrong difficulty is fatal" $ witnessProperty $ do
        _ <- stop $ pendingWith "To be resolved in [DSCP-261]"

        n <- pick $ getPositive <$> arbitrary
        let blocks = makeBlocksChain n
        let spoilBlock block = do
                badDiff <- arbitrary
                return $ block & bHeaderL . hDifficultyL .~ badDiff
                               & resignBlock
        submitSpoiledChain blocks spoilBlock

    it "Block slot only increases over time" $ witnessProperty $ do
        _ <- stop $ pendingWith "To be resolved in [DSCP-261]"

        issuer <- lift $ getSecretKey @WitnessNode
        n <- pick $ (+1) . getPositive <$> arbitrary
        -- going to modify block before the last one with too high slotId
        let issuerAddr = mkAddr $ toPublic issuer
            blocks = makeBlocksChain n
            ownedSlot = committeeOwnsSlot testCommittee issuerAddr
            lastSlot = blocks ^. to last . bHeaderL . hSlotIdL
            futureSlot = L.head $ filter ownedSlot [99999..]
        oddSlot <- pick $ elements [lastSlot, futureSlot]
        let badBlocks = blocks & ix (n - 2) %~
                          \block -> block & bHeaderL . hSlotIdL .~ oddSlot
                                          & resignBlock
        lift $ do
            mapM_ submitBlock (init badBlocks)
            throwsSome $ submitBlock (last badBlocks)

    it "Wrong signature is not fine" $ witnessProperty $ do
        let block1 :| [] = makeBlocksChain 1
        block2 <- pick arbitrary
        mixBlock <- pick $ arbitraryUniqueMixture block1 block2
        lift . throwsSome $ submitBlock mixBlock

  describe "Block content" $ do
    it "Content is verified" $ witnessProperty $ do
        let block :| [] = makeBlocksChain 1
        tx <- pick arbitrary
        let badTx = tx & twTxL . txInAccL . tiaNonceL .~ 999
        let badBlock = block & bBodyL . bbTxsL .~ one (GMoneyTxWitnessed badTx)
        lift . throwsSome $ submitBlock badBlock

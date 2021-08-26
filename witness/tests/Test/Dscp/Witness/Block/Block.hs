module Test.Dscp.Witness.Block.Block where

import Control.Lens (has)
import qualified Data.List as L
import qualified GHC.Exts as Exts
import Test.QuickCheck.Monadic (pre)

import Dscp.Core
import Dscp.Crypto
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Types
import Dscp.Util.Test
import Dscp.Witness
import Test.Dscp.Witness.Mode

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
    resignBlockAs (testFindSlotOwner $ hSlotId $ bHeader block) block

-- | Submit block to validation.
submitBlock
    :: (HasWitnessConfig, WithinWriteSDLock)
    => Block -> WitnessTestMode' ()
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
            let hSlotId = mkSlotId i
                hDifficulty = fromIntegral i
                issuer = testFindSlotOwner hSlotId
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
    mkSlotId blockNo = fromIntegral $ blockNo * 3 + ((blockNo * 231) `mod` 3)

-- | Given correct chain, and spoiling function, modifies one of blocks
-- in a chain and checks that its application fails.
submitSpoiledChain
    :: (HasWitnessConfig, WithinWriteSDLock, Exception e)
    => (e -> Bool)
    -> NonEmpty Block
    -> (Block -> Gen Block)
    -> PropertyM WitnessTestMode' Property
submitSpoiledChain isExpectedError chain spoilBlock = do
    let lastBlock = last chain
    badLastBlock <- pick $ spoilBlock lastBlock
    pre (badLastBlock /= lastBlock)
    lift $ do
        mapM_ submitBlock (init chain)
        throwsMatching isExpectedError $ submitBlock badLastBlock

spec_Block_validation_with_application :: Spec
spec_Block_validation_with_application = do
    describe "Block header" $ do
        it "Good block is fine" $ witnessProperty $ do
            let blocks = makeBlocksChain 1
            lift . noThrow $ mapM_ submitBlock blocks

        it "Several good blocks are is fine" $ witnessProperty $ do
            n <- pick $ choose (1, 5)
            let blocks = makeBlocksChain n
            lift . noThrow $ mapM_ submitBlock blocks

        it "Wrong previous block hash is not fine" $ witnessProperty $ do
            n <- pick $ choose (1, 5)
            let blocks = makeBlocksChain n
            let spoilBlock block = do
                    badPrevHash <- arbitrary
                    return $ block & bHeaderL . hPrevHashL .~ badPrevHash
                                   & resignBlock
            submitSpoiledChain (\SomeException{} -> True) blocks spoilBlock

        it "Applying block not by committee member is not fine" $ witnessProperty $ do
            issuer <- pick arbitrary
            let block :| [] = makeBlocksChain 1
                badBlock = block & resignBlockAs issuer
            pre (badBlock /= block)
            lift . throwsSome $ submitBlock badBlock

        it "Wrong difficulty is fatal" $ witnessProperty $ do
            n <- pick $ choose (1, 5)
            let blocks = makeBlocksChain n
            let spoilBlock block = do
                    badDiff <- arbitrary
                    return $ block & bHeaderL . hDifficultyL .~ badDiff
                                   & resignBlock
            let isExpectedError = or . sequence
                                  [ has (_BlockError . _DuplicatedDifficulty)
                                  , has (_BlockError . _DifficultyIsTooLarge)
                                  ]
            submitSpoiledChain isExpectedError blocks spoilBlock

        it "Block slot only increases over time" $ witnessProperty $ do
            n <- pick $ choose (2, 5)
            -- going to modify block before the last one with too high slotId
            let blocks = makeBlocksChain n
                preLastBlock = L.last $ init blocks
            let spoilBlock block = do
                    oddSlot <- choose (0, hSlotId (bHeader preLastBlock) - 1)
                    return $ block & bHeaderL . hSlotIdL .~ oddSlot
                                   & resignBlock
            submitSpoiledChain (has $ _BlockError . _SlotIdIsNotIncreased)
                blocks spoilBlock

        it "Wrong signature is not fine" $ witnessProperty $ do
            let block1 :| [] = makeBlocksChain 1
            block2 <- pick arbitrary
            mixBlock <- pick $ arbitraryUniqueMixture block1 block2
            lift . throwsSome $ submitBlock mixBlock

    describe "Block content" $
        it "Content is verified" $ witnessProperty $ do
            let block :| [] = makeBlocksChain 1
            tx <- pick arbitrary
            let badTx = tx & twTxL . txInAccL . tiaNonceL .~ 999
            let badBlock = block & bBodyL . bbTxsL .~ one (GMoneyTxWitnessed badTx)
            lift . throwsSome $ submitBlock badBlock

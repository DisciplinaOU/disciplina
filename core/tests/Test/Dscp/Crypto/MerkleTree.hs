module Test.Dscp.Crypto.MerkleTree where

import qualified Data.List as List
import qualified Data.Set as Set

import Dscp.Crypto
import Dscp.Util.Test

longerThan :: Container f => Int -> f -> Bool
longerThan n x = length x > n

instance (HasHash a, Arbitrary a) => Arbitrary (MerkleTree a) where
    arbitrary = fromList <$> arbitrary

instance (HasHash a, Arbitrary a) => Arbitrary (MerkleNode a) where
    arbitrary = arbitrary `suchThat` longerThan 0 >>= \case
        MerkleTree node -> pure node
        _ -> error "impossible"

instance (HasHash a, Arbitrary a) => Arbitrary (MerkleProof a) where
    arbitrary = do
        tree <- MerkleTree <$> arbitrary
        let n = length tree
        idxs <- sublistOf [(0 :: Word32) .. fromIntegral n - 1]
                `suchThat` longerThan 0
        maybe (error "impossible") pure $
            mkMerkleProof tree $ Set.fromList idxs

spec_merkleTree :: Spec
spec_merkleTree = describe "Merkle Tree Tests" $ do
    it "should preserve leaf order when constructed from Foldable" $ property $
      \(xs :: [Int]) -> toList (fromFoldable xs) == xs

    it "should preserve leaf order when constructed from Container" $ property $
      \(xs :: String) -> toList (fromContainer xs) == xs

    it "should have correct length" $ property $
       \(xs :: [ByteString]) ->
         length (fromFoldable xs) `shouldBe` length xs

    it "should have correct size" $ property $
       \(xs :: [ByteString]) ->
         mrSize (getMerkleRoot (fromFoldable xs))
          `shouldBe` fromIntegral (length xs)

    it "should have correct length and size even for empty tree" $ do
         mrSize (getMerkleRoot MerkleEmpty) `shouldBe` 0
         length MerkleEmpty `shouldBe` 0

    it "should have equal length and size" $ property $
       \(xs :: String) ->
         mrSize (getMerkleRoot (fromFoldable xs))
            `shouldBe` fromIntegral (length xs)

    it "can construct and verify single proofs " $ property $
       \(xs :: [Int], leafIdx) ->
        let tree = fromFoldable xs
        in (validateMerkleProof <$> mkMerkleProofSingle tree leafIdx <*> Just (getMerkleRoot tree))
            `shouldBe` if leafIdx < fromIntegral (length xs) && leafIdx >= 0
                       then Just True
                       else Nothing

    it "can construct and verify Set proofs " $ property $
       \(treeLeafs :: [Int], proofIndicies' :: [Word32]) ->
        let tree = fromFoldable treeLeafs
            proofIndicies = Set.fromList proofIndicies'
            haveLeafIndex = any (\x -> x < fromIntegral (length treeLeafs) && x >= 0) proofIndicies'
        in (validateMerkleProof <$> mkMerkleProof tree proofIndicies <*> Just (getMerkleRoot tree))
            `shouldBe` if haveLeafIndex then Just True else Nothing

    it "performs lookup correctly" $ property $
      \(Fixed (xs' :: [ByteString], indices :: [Word32])) ->
        let xs          = List.nub xs'
            withIndices = zip [0..] xs
            tree        = fromList xs
            count       = fromIntegral $ length tree
            uniqIndices = Set.fromList $ map (`mod` count) $ List.nub indices
            proof'      = mkMerkleProof tree uniqIndices

            (pairsToLook, pairsToFail) =
                List.partition
                    ((`elem` uniqIndices) . fst)
                    withIndices

            shouldBeFoundIn    proof (index, value) = Just value == lookup index proof
            shouldBeNotFoundIn proof (index, _    ) = isNothing $ lookup index proof

        in  case proof' of
                Nothing    -> True
                Just proof ->
                       all (shouldBeFoundIn    proof) pairsToLook
                    && all (shouldBeNotFoundIn proof) pairsToFail

    it "splits and merges proofs and its data correctly" $ property $
        \(proof :: MerkleProof Int) ->
            uncurry mergeProofAndData (separateProofAndData proof) === Just proof

module Test.Disciplina.Crypto.MerkleTree where

import Test.Common
import Disciplina.Crypto (MerkleTree (..), mkMerkleProofSingle
                         ,validateMerkleProof,fromFoldable
                         ,fromContainer, mrSize, getMerkleRoot)

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
       \(xs :: [Char]) ->
         mrSize (getMerkleRoot (fromFoldable xs))
            `shouldBe` fromIntegral (length xs)

    it "can construct and verify proofs " $ property $
       \(xs :: [Int], leafIdx) ->
        let tree = fromFoldable xs
        in (validateMerkleProof <$> mkMerkleProofSingle tree leafIdx <*> Just (getMerkleRoot tree))
            `shouldBe` if (leafIdx < length xs && leafIdx >= 0) == True
                       then Just True
                       else Nothing

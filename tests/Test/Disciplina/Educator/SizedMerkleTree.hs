module Test.Disciplina.Educator.SizedMerkleTree where

import Test.Common
import Disciplina.Educator (MerkleTree(..), MerkleProof(..)
                           ,mkMerkleProof, validateMerkleProof, drawMerkleTree
                           ,fromFoldable, fromContainer, smrSize, smrHash
                           ,getMerkleRoot)

spec_merkleTree :: Spec
spec_merkleTree = describe "Merkle Tree tests" $ do
    it "should construct tree" $ do
        toList (fromFoldable leafsTree1) `shouldBe` leafsTree1
        toList (fromContainer leafsTree2) `shouldBe` "abcde"
        smrHash (getMerkleRoot testTree1)
          `shouldBe` smrHash (getMerkleRoot testTree2)

    it "should have properties" $ do
        length testTree1 `shouldBe` length leafsTree1
        smrSize (getMerkleRoot testTree1)
          `shouldBe` fromIntegral (length leafsTree1)

    it "should validate merkle tree proof" $ do
        validateMerkleProof (mkMerkleProof testTree1 0) testTree1
          `shouldBe` True
        validateMerkleProof (mkMerkleProof testTree1 4) testTree1
          `shouldBe` True
        validateMerkleProof (mkMerkleProof testTree1 5) testTree1
          `shouldBe` False

leafsTree1 :: [Text]
leafsTree1 = ["a","b","c","d","e"]

testTree1 :: MerkleTree Text
testTree1 = fromFoldable leafsTree1

leafsTree2 :: Text
leafsTree2 = "abcde"

testTree2 :: MerkleTree Char
testTree2 = fromContainer leafsTree2


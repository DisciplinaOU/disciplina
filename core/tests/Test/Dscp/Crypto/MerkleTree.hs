module Test.Dscp.Crypto.MerkleTree where

import Universum
import GHC.Exts (fromList)

import qualified Data.List as List
import qualified Data.Set as Set

import Dscp.Crypto
import Dscp.Util.Test

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
            msSize (getMerkleRoot (fromFoldable xs))
            `shouldBe` fromIntegral (length xs)

    it "should have correct length and size even for empty tree" $ do
        msSize (getMerkleRoot MerkleEmpty) `shouldBe` 0
        length MerkleEmpty `shouldBe` 0

    it "should have equal length and size" $ property $
        \(xs :: String) ->
            msSize (getMerkleRoot (fromFoldable xs))
            `shouldBe` fromIntegral (length xs)

    it "estimates the proof size correctly (equal to original tree's size)" $ property $
        \(xs :: [Int], idxs :: Set Word32) ->
            let tree = fromList xs
                mProof = mkMerkleProof tree idxs
            in case mProof of
                   Nothing    -> property True
                   Just proof -> length tree === fromIntegral (mpSize proof)

    it "can construct and verify single proofs " $ property $
        \(xs :: [Int], leafIdx) ->
            let tree = fromFoldable xs
                validationRes = validateMerkleProof <$>
                                (readyProof <$> mkMerkleProofSingle tree leafIdx) <*>
                                Just (getMerkleRoot tree)
            in validationRes `shouldBe` if leafIdx < fromIntegral (length xs) && leafIdx >= 0
                                        then Just True
                                        else Nothing

    it "can construct and verify Set proofs " $ property $
        \(treeLeafs :: [Int], proofIndicies' :: [Word32]) ->
            let tree = fromFoldable treeLeafs
                proofIndicies = Set.fromList proofIndicies'
                haveLeafIndex = any (\x -> x < fromIntegral (length treeLeafs) && x >= 0) proofIndicies'
                validationRes = validateMerkleProof <$>
                                (readyProof <$> mkMerkleProof tree proofIndicies) <*>
                                Just (getMerkleRoot tree)
            in validationRes `shouldBe` if haveLeafIndex then Just True else Nothing

    it "performs lookup correctly" $ property $
        \(Fixed (xs' :: [ByteString], indices :: [Word32])) ->
            let xs          = List.nub xs'
                withIndices = zip [0..] xs
                tree        = fromList xs
                count       = fromIntegral $ length tree
                uniqIndices = Set.fromList $ map (`mod` count) $ List.nub indices
                proof'      = mkMerkleProof tree uniqIndices

                (pairsToLook, pairsToFail) = List.partition
                    ((`elem` uniqIndices) . fst)
                    withIndices

                shouldBeFoundIn    proof (index, value) = Just value == lookup index proof
                shouldBeNotFoundIn proof (index, _    ) = isNothing $ lookup index proof

            in case proof' of
                Nothing    -> property True
                Just proof ->
                    counterexample "Not found indices which should be found"
                    (all (shouldBeFoundIn    proof) pairsToLook)
                    .&&.
                    counterexample "Found some indices which should not be found"
                    (all (shouldBeNotFoundIn proof) pairsToFail)

    it "splits and merges proofs and its data correctly" $ property $
        \(proof :: MerkleProof Int) ->
            uncurry mergeProofAndData (separateProofAndData proof) === Just proof

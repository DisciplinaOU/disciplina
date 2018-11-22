module Dscp.Core.FairCV
       (
         -- * Tagged proofs
         TaggedProof
       , Unchecked
       , Valid
       , mkTaggedProof
       , unTaggedProof

       , validateTaggedProof
       , mergeProofs

         -- * Fair CV
       , FairCV (..)
       , validateFairCV
       , singletonFCV
       , mergeFairCVs
       , addProof

         -- * Fair CV check result
       , FairCVCheckResult (..)
       ) where

import qualified Data.Map.Merge.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Dscp.Core.Foundation
import Dscp.Crypto

---------------------------------------------------------------------------
-- Tagged Merkle proofs
---------------------------------------------------------------------------

-- | Type tag for proofs/CVs not checked yet.
data Unchecked

-- | Type tag for validated proofs/CVs.
data Valid

-- | 'MerkleProof' tagged by validation tag. When using this module,
-- it is impossible to obtain a value of 'TaggedProof Valid a' without
-- doing a validity check.
newtype TaggedProof v a = TaggedProof
    { unTaggedProof :: MerkleProof a
    } deriving (Show, Eq, Generic)

-- | Safe constructor for 'TaggedProof', which can yield only 'Unckecked'
-- proof.
mkTaggedProof :: MerkleProof a -> TaggedProof Unchecked a
mkTaggedProof = TaggedProof

-- | If 'Unchecked' proof is valid, make it 'Valid', otherwise return an
-- error.
validateTaggedProof
    :: HasHash a
    => TaggedProof Unchecked a
    -> Either Text (TaggedProof Valid a)
validateTaggedProof (TaggedProof unchecked) =
    if isJust (computeMerkleRoot unchecked)
    then Right $ TaggedProof unchecked
    else Left "Proof validation has failed"

-- | Merges two already validated Merkle proofs, checking that all common
-- inner nodes match.
mergeProofs
    :: TaggedProof Valid a
    -> TaggedProof Valid a
    -> Either Text (TaggedProof Valid a)
mergeProofs (TaggedProof a) (TaggedProof b)
    | pnSig a /= pnSig b = Left "Node signatures mismatch"
    | otherwise = TaggedProof <$> merge a b
  where
    merge (ProofBranch s l r) (ProofBranch _ l' r') =
        let mergeUntag t t' =
                unTaggedProof <$>
                mergeProofs (TaggedProof t) (TaggedProof t')
        in ProofBranch s <$> mergeUntag l l' <*> mergeUntag r r'
    merge ProofBranch{} ProofLeaf{} =
        Left "Inner node on the left is mismatched with leaf on the right"
    merge ProofLeaf{} ProofBranch{} =
        Left "Leaf on the left is mismatched with inner node on the right"
    -- No need to compare values in leaves, because proofs are 'Valid',
    -- and hash function should be collision-resistnant.
    merge a'@ProofLeaf{} ProofLeaf{} = pure a'
    merge a' ProofPruned{} = pure a'
    merge ProofPruned{} b' = pure b'

---------------------------------------------------------------------------
-- FairCV
---------------------------------------------------------------------------

-- | FairCV datatype. Proofs are divided by educators (designated by their
-- addresses) and blocks (designated by their hashes).
-- Phantom type parameter @v@ distinguishes between already validated
-- 'FairCV' and 'FairCV' not validated yet.
newtype FairCV v = FairCV
    { unFairCV :: Map Address (Map PrivateHeaderHash (TaggedProof v PrivateTx))
    } deriving (Show, Eq, Generic)

-- | If 'Unchecked' FairCV is valid, make it 'Valid', otherwise return an
-- error.
validateFairCV :: FairCV Unchecked -> Either Text (FairCV Valid)
validateFairCV (FairCV cv) =
    FairCV <$> traverse (traverse validateTaggedProof) cv

-- | Make a FairCV from one proof.
singletonFCV
    :: Address
    -> PrivateHeaderHash
    -> TaggedProof v PrivateTx
    -> FairCV v
singletonFCV educatorAddr blkHash proof = FairCV $
    M.singleton educatorAddr $
    M.singleton blkHash proof

-- | Merge two FairCVs, checking if their common parts match.
mergeFairCVs :: FairCV Valid -> FairCV Valid -> Either Text (FairCV Valid)
mergeFairCVs (FairCV a) (FairCV b) = FairCV <$>
    unionWithA (unionWithA mergeProofs) a b
  where
    unionWithA f =
        M.mergeA M.preserveMissing M.preserveMissing $
        M.zipWithAMatched $ const f

-- | Adds a single Merkle proof to the FairCV.
addProof
    :: Address
    -> PrivateHeaderHash
    -> TaggedProof Valid PrivateTx
    -> FairCV Valid
    -> Either Text (FairCV Valid)
addProof educatorAddr blkHash proof =
    mergeFairCVs $ singletonFCV educatorAddr blkHash proof

---------------------------------------------------------------------------
-- Fair CV check result
---------------------------------------------------------------------------

newtype FairCVCheckResult = FairCVCheckResult
    { unFairCVCheckResult :: Map Address (Map PrivateHeaderHash Bool)
    } deriving (Show, Eq, Generic)

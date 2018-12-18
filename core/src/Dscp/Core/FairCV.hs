-- | Datatypes and functions related to FairCV functionality.
module Dscp.Core.FairCV
       (
         -- * Fair CV
         FairCV (..)
       , FairCVReady (..)
       , readyFairCV
       , unReadyFairCV
       , singletonFCV
       , mergeFairCVs
       , addProof

         -- * Fair CV check result
       , FairCVCheckResult (..)
       ) where

import qualified Data.Map.Merge.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Fmt (build, mapF, (+|), (|+))

import Dscp.Core.Foundation
import Dscp.Crypto

-- | Two-level map which represents common structure for all `FairCV*`
-- types
type GenericFairCV a = Map Address (Map PrivateHeaderHash a)

-- | FairCV datatype. Proofs are divided by educators (designated by their
-- addresses) and blocks (designated by their hashes).
newtype FairCV = FairCV
    { unFairCV :: GenericFairCV (MerkleProof PrivateTx)
    } deriving (Show, Eq, Generic)

instance Buildable FairCV where
    build (FairCV cv) = "FairCV { "+|mapF (mapF <$> cv)|+" }"

-- | @'FairCV'@ with pre-processed proofs (all the proofs have their Merkle
-- roots pre-calculated).
newtype FairCVReady = FairCVReady
    { unFairCVReady :: GenericFairCV (MerkleProofReady PrivateTx)
    } deriving (Show, Eq, Generic)

instance Buildable FairCVReady where
    build (FairCVReady cv) = "FairCVReady { "+|mapF (mapF <$> cv)|+" }"

-- | Pre-process all the proofs in @'FairCV'@
readyFairCV :: FairCV -> FairCVReady
readyFairCV = FairCVReady . fmap (fmap readyProof) . unFairCV

-- | Strip roots from all the proofs in @'FairCVReady'@
unReadyFairCV :: FairCVReady -> FairCV
unReadyFairCV = FairCV . fmap (fmap mprProof) . unFairCVReady

-- | Make a FairCV from one proof.
singletonFCV
    :: Address
    -> PrivateHeaderHash
    -> MerkleProofReady PrivateTx
    -> FairCVReady
singletonFCV educatorAddr blkHash proof = FairCVReady $
    M.singleton educatorAddr $
    M.singleton blkHash proof

-- | Merge two pre-processed FairCVs, checking if their common parts match.
mergeFairCVs :: FairCVReady -> FairCVReady -> Either Text FairCVReady
mergeFairCVs (FairCVReady a) (FairCVReady b) = FairCVReady <$>
    unionWithA (unionWithA mergeProofs) a b
  where
    unionWithA f =
        M.mergeA M.preserveMissing M.preserveMissing $
        M.zipWithAMatched $ const f

-- | Adds a single Merkle proof to the FairCV.
addProof
    :: Address
    -> PrivateHeaderHash
    -> MerkleProofReady PrivateTx
    -> FairCVReady
    -> Either Text FairCVReady
addProof educatorAddr blkHash proof =
    mergeFairCVs $ singletonFCV educatorAddr blkHash proof

---------------------------------------------------------------------------
-- Fair CV check result
---------------------------------------------------------------------------

data FairCVCheckResult = FairCVCheckResult
    { fairCVCheckResults :: GenericFairCV Bool
    , fairCVFullyValid   :: Bool
    } deriving (Show, Eq, Generic)

instance Buildable FairCVCheckResult where
    build (FairCVCheckResult res total) =
        "Fair CV check result: "+|totalS+|" "+|mapF (mapF <$> res)
      where
        totalS = if total then "✔" else "✘"

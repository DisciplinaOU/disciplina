-- | Datatypes and functions related to FairCV functionality.
module Dscp.Core.FairCV
       (
         -- * Fair CV
         FairCVTemplate (..)
       , fcStudentAddrL
       , fcStudentNameL
       , fcCVL
       , FairCV
       , FairCVReady
       , readyFairCV
       , unReadyFairCV
       , singletonFCV
       , mergeFairCVs
       , addProof
       , privateBlockToFairCV

         -- * Fair CV check result
       , FairCVCheckResult (..)
       , FairCVAndCheckResult (..)
       ) where

import Universum
import Control.Lens (makeLensesWith)
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import Fmt (Buildable (..), mapF, (+|), (|+))

import Dscp.Core.PubChain
import Dscp.Core.Foundation
import Dscp.Crypto
import Dscp.Util

-- | Two-level map which represents common structure for all `FairCV*`
-- types
type GenericFairCV a = Map PubAddress (Map PrivateHeaderHash a)

-- | FairCV template, which contains the common data for both
-- unprocessed and pre-processed FairCVs.
data FairCVTemplate proof = FairCV
    { fcStudentAddr :: !Address
    , fcStudentName :: !Text
    , fcCV          :: !(GenericFairCV proof)
    } deriving (Show, Eq, Generic)

makeLensesWith postfixLFields ''FairCVTemplate

instance Buildable proof => Buildable (FairCVTemplate proof) where
    build (FairCV addr name cv) =
        "FairCV { student = ("+|name|+", "+|addr|+
        "), cv = "+|mapF (mapF <$> cv)|+" }"

-- | FairCV datatype. Proofs are divided by educators (designated by their
-- addresses) and blocks (designated by their hashes).
type FairCV = FairCVTemplate (MerkleProof PrivateTx)

-- | @'FairCV'@ with pre-processed proofs (all the proofs have their Merkle
-- roots pre-calculated).
type FairCVReady = FairCVTemplate (MerkleProofReady PrivateTx)

-- | Pre-process all the proofs in @'FairCV'@
readyFairCV :: FairCV -> FairCVReady
readyFairCV = fcCVL %~ fmap (fmap readyProof)

-- | Strip roots from all the proofs in @'FairCVReady'@
unReadyFairCV :: FairCVReady -> FairCV
unReadyFairCV = fcCVL %~ fmap (fmap mprProof)

-- | Make a FairCV from one proof.
singletonFCV
    :: Address                    -- ^ Student's address
    -> Text                       -- ^ Student's name
    -> PubAddress                 -- ^ Educator's address
    -> PrivateHeaderHash          -- ^ Private block header hash
    -> MerkleProofReady PrivateTx -- ^ Merkle proof
    -> FairCVReady
singletonFCV sAddr sName educatorAddr blkHash proof =
    FairCV sAddr sName $
    M.singleton educatorAddr $
    M.singleton blkHash proof

-- | Merge two pre-processed FairCVs, checking if their common parts match.
-- Does not check that student names match (student names are metadata,
-- which should eventually be provided on-chain or fixed by some hash).
mergeFairCVs :: FairCVReady -> FairCVReady -> Either Text FairCVReady
mergeFairCVs (FairCV aAddr aName a) (FairCV bAddr _ b)
    | aAddr /= bAddr = Left "Student's addresses do not match"
    | otherwise = FairCV aAddr aName <$>
                  unionWithA (unionWithA mergeProofs) a b
  where
    unionWithA f =
        M.mergeA M.preserveMissing M.preserveMissing $
        M.zipWithAMatched $ const f

-- | Adds a single Merkle proof to the FairCV.
addProof
    :: PubAddress
    -> PrivateHeaderHash
    -> MerkleProofReady PrivateTx
    -> FairCVReady
    -> Either Text FairCVReady
addProof educatorAddr blkHash proof fcv@(FairCV sAddr sName _) =
    mergeFairCVs fcv $
    singletonFCV sAddr sName educatorAddr blkHash proof

-- | Make a FairCV from one private block.
privateBlockToFairCV
    :: PrivateBlockHeader
    -> NonEmpty PrivateTx
    -> PubAddress
    -> (Address, Text)
    -> FairCVReady
privateBlockToFairCV blkHeader txs educator (student, studentName) =
    let blkHash = hash blkHeader
        proof = readyProof $ merkleProofFromList txs
    in singletonFCV student studentName educator blkHash proof

---------------------------------------------------------------------------
-- Fair CV check result
---------------------------------------------------------------------------

data FairCVCheckResult = FairCVCheckResult
    { fairCVCheckResults :: GenericFairCV Bool
    , fairCVFullyValid   :: Bool
    } deriving (Show, Eq, Generic)

data FairCVAndCheckResult = FairCVAndCheckResult
    { fcacrFairCV      :: FairCVReady
    , fcacrCheckResult :: FairCVCheckResult
    } deriving (Eq, Show, Generic)

instance Buildable FairCVCheckResult where
    build (FairCVCheckResult res total) =
        "Fair CV check result: "+|totalS+|" "+|mapF (mapF <$> res)
      where
        totalS = if total then "✔" else "✘"

instance Buildable FairCVAndCheckResult where
    build (FairCVAndCheckResult res fcv) =
        "{ fairCV = "  +| res |+
        ", checkResult = " +| fcv |+
        " }"

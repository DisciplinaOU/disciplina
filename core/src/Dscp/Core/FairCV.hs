{-# LANGUAGE DeriveFunctor #-}

-- | Datatypes and functions related to FairCV functionality.
module Dscp.Core.FairCV
       (
         -- * Fair CV
         FairCVTemplate (..)
       , TxIdAnnotated (..)
       , tiaTxIdL
       , tiaValL
       , fcDescL
       , fcCVL
       , FairCV
       , FairCVReady
       , readyFairCV
       , unReadyFairCV
       , annotateWithTxId
       , singletonFCV
       , mergeFairCVs
       , addProof
       , privateBlockToFairCV
       , extractContentsFromFairCV

         -- * Fair CV check result
       , FairCVCheckResult (..)
       , FairCVAndCheckResult (..)
       ) where

import Universum

import Control.Lens (makeLensesWith, (?~))
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import Fmt (Buildable (..), mapF, (+|), (|+))

import Dscp.Core.Foundation
import Dscp.Core.PubChain
import Dscp.Crypto
import Dscp.Util

-- | Datatype which provides a public transaction ID alongside the
-- block proof, if it exists
data TxIdAnnotated a = TxIdAnnotated
  { tiaTxId :: Maybe PubTxId
  , tiaVal  :: a
  } deriving (Show, Eq, Generic, Functor)

withoutTxId :: a -> TxIdAnnotated a
withoutTxId = TxIdAnnotated Nothing


-- | Two-level map which represents common structure for all `FairCV*`
-- types
type GenericFairCV a = Map PubAddress (Map PrivateHeaderHash (TxIdAnnotated a))

-- | FairCV template, which contains the common data for both
-- unprocessed and pre-processed FairCVs.
data FairCVTemplate proof = FairCV
    { fcDesc  :: !Text
    , fcCV    :: !(GenericFairCV proof)
    } deriving (Show, Eq, Generic)

makeLensesWith postfixLFields ''TxIdAnnotated
makeLensesWith postfixLFields ''FairCVTemplate

instance Buildable a => Buildable (TxIdAnnotated a) where
    build (TxIdAnnotated txId val) =
        "TxIdAnnotated { txId = "+|txId|+", val = "+|val|+" }"

instance Buildable proof => Buildable (FairCVTemplate proof) where
    build (FairCV desc cv) =
        "FairCV { description = ("+|desc|+
        "), cv = "+|mapF (mapF <$> cv)|+" }"

-- | FairCV datatype. Proofs are divided by educators (designated by their
-- addresses) and blocks (designated by their hashes).
type FairCV = FairCVTemplate (MerkleProof PrivateTx)

-- | @'FairCV'@ with pre-processed proofs (all the proofs have their Merkle
-- roots pre-calculated).
type FairCVReady = FairCVTemplate (MerkleProofReady PrivateTx)

-- | @'FairCV' with transactions extracted
type FairCVContents = FairCVTemplate [PrivateTx]


-- | Helper function: maps annotated values inside FairCV
mapProofs
  :: (TxIdAnnotated a -> TxIdAnnotated b)
  -> FairCVTemplate a -> FairCVTemplate b
mapProofs f = fcCVL %~ fmap (fmap f)

-- | Pre-process all the proofs in @'FairCV'@
readyFairCV :: FairCV -> FairCVReady
readyFairCV = mapProofs $ fmap readyProof

-- | Strip roots from all the proofs in @'FairCVReady'@
unReadyFairCV :: FairCVReady -> FairCV
unReadyFairCV = mapProofs $ fmap mprProof

-- | Put `PubTxId` into every proof root of the FairCV.
-- TODO: enough for certificates, not enough for more complex
-- use cases.
annotateWithTxId :: PubTxId -> FairCVTemplate proof -> FairCVTemplate proof
annotateWithTxId txId = mapProofs $ tiaTxIdL ?~ txId

-- | Make a FairCV from one proof.
singletonFCV
    :: Text                       -- ^ Description
    -> PubAddress                 -- ^ Educator's address
    -> PrivateHeaderHash          -- ^ Private block header hash
    -> MerkleProofReady PrivateTx -- ^ Merkle proof
    -> FairCVReady
singletonFCV desc educatorAddr blkHash proof =
    FairCV desc $
    M.singleton educatorAddr $
    M.singleton blkHash $ withoutTxId proof

-- | Merge two pre-processed FairCVs, checking if their common parts match.
-- Does not check that student names match (student names are metadata,
-- which should eventually be provided on-chain or fixed by some hash).
mergeFairCVs :: FairCVReady -> FairCVReady -> Either Text FairCVReady
mergeFairCVs (FairCV desc a) (FairCV _ b) = FairCV desc <$>
    unionWithA (unionWithA mergeAnnotatedProofs) a b
  where
    unionWithA f =
        M.mergeA M.preserveMissing M.preserveMissing $
        M.zipWithAMatched $ const f

    mergeAnnotatedProofs (TxIdAnnotated tx1 a') (TxIdAnnotated tx2 b') =
        equalIfNotMissing tx1 tx2 >>= \tx -> TxIdAnnotated tx <$> mergeProofs a' b'

    equalIfNotMissing Nothing Nothing  = Right Nothing
    equalIfNotMissing (Just a') Nothing = Right (Just a')
    equalIfNotMissing Nothing (Just b') = Right (Just b')
    equalIfNotMissing (Just a') (Just b') =
        if a' == b' then Right (Just a') else Left "Transaction IDs do not match"

-- | Adds a single Merkle proof to the FairCV.
addProof
    :: PubAddress
    -> PrivateHeaderHash
    -> MerkleProofReady PrivateTx
    -> FairCVReady
    -> Either Text FairCVReady
addProof educatorAddr blkHash proof fcv@(FairCV desc _) =
    mergeFairCVs fcv $
    singletonFCV desc educatorAddr blkHash proof

-- | Make a FairCV from one private block.
privateBlockToFairCV
    :: PrivateBlockHeader
    -> NonEmpty PrivateTx
    -> PubAddress
    -> Text
    -> FairCVReady
privateBlockToFairCV blkHeader txs educator desc =
    let blkHash = hash blkHeader
        proof = readyProof $ merkleProofFromList txs
    in singletonFCV desc educator blkHash proof

-- | Extract contents from FairCV (without proofs)
extractContentsFromFairCV :: FairCV -> FairCVContents
extractContentsFromFairCV = over fcCVL $ (fmap . fmap . fmap) toList


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

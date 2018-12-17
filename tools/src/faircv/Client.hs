module Client where

-- import Data.Traversable (for)

-- import Dscp.Core
-- import Dscp.Crypto
-- import Dscp.Educator.Web.Student
-- import Dscp.Educator.Web.Types
-- import Dscp.Util
-- import Dscp.Util.Aeson

-- import Test.QuickCheck

-- data InvalidProofs = InvalidProofs deriving (Show, Exception)

-- -- | Get all proofs since given time.
-- getProofs :: StudentApiClient -> IO [MerkleProof PrivateTx]
-- getProofs sc = do
--     rawProofs <- sGetProofs sc Nothing False
--     let zipProof BlkProofInfo {..} = mergeProofAndData
--             (unEncodeSerialised bpiMtreeSerialized)
--             bpiTxs
--     nothingToThrow InvalidProofs $
--         mapM zipProof rawProofs

-- getAssignments :: StudentApiClient -> IO [AssignmentStudentInfo]
-- getAssignments sc = do
--     hashes <- map aiHash <$> sGetAssignments sc Nothing Nothing Nothing False
--     for hashes $ sGetAssignment sc

-- makeRandomSubmissionForAssignment :: StudentApiClient -> Hash Assignment -> IO NewSubmission
-- makeRandomSubmissionForAssignment sc aHash = do
--     return NewSubmission
--         { nsAssignmentHash = aHash
--         , nsContentsHash   = hash "mkay"
--         , nsWitness        = _
--         }

-- getAllCourses :: StudentApiClient -> IO [Course]
-- getAllCourses sc = do
--     map ciId <$> sGetCourses sc Nothing False


import Dscp.Core
import Dscp.Witness.Web.Client

import Test.QuickCheck

getFairCV :: IO FairCV
getFairCV = generate arbitrary

checkFairCV :: WitnessClient -> FairCV -> IO FairCVCheckResult
checkFairCV wc fairCV =
    wCheckFairCV wc fairCV

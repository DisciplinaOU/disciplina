module FaircvClient where

import Data.Traversable (for)

import Data.Default (def)
import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Student
import Dscp.Educator.Web.Types
import Dscp.Util
import Dscp.Util.Aeson
import Dscp.Witness.Web.Client

import Test.QuickCheck

data InvalidProofs = InvalidProofs deriving (Show, Exception)

-- | Get all proofs since given time.
getProofs :: StudentApiClientNoAuth -> IO [MerkleProof PrivateTx]
getProofs sc = do
    rawProofs <- sGetProofs sc False def
    nothingToThrow InvalidProofs $ mapM zipProof rawProofs

zipProof :: BlkProofInfo -> Maybe (MerkleProof PrivateTx)
zipProof BlkProofInfo {..} =
    mergeProofAndData (unEncodeSerialised bpiMtreeSerialized) bpiTxs

blkToFairCV :: Address -> Text -> Address -> BlkProofInfo -> IO FairCVReady
blkToFairCV studentAddr studentName educatorAddr proof = do
    zippedProof <- nothingToThrow InvalidProofs $ zipProof proof
    return $ singletonFCV studentAddr studentName educatorAddr
        (bpiBlockHash proof) (readyProof zippedProof)

-- Merge a list of 'FairCVReady' into a single one
mergeFairCVList :: [FairCVReady] -> Either Text FairCVReady
mergeFairCVList [] = Left "No FairCV in the list"
mergeFairCVList (fcv:fcvs) =
    foldr (\fa efb -> efb >>= mergeFairCVs fa) (Right fcv) fcvs

getAssignments :: StudentApiClientNoAuth -> IO [AssignmentStudentInfo]
getAssignments sc = do
    hashes <- map aiHash <$> sGetAssignments sc False def def def
    for hashes $ sGetAssignment sc

makeRandomSubmissionForAssignment :: SecretKey -> Hash Assignment -> IO NewSubmission
makeRandomSubmissionForAssignment student aHash = do
    ch <- generate arbitrary

    let sub = Submission
            { _sStudentId = mkAddr (toPublic student)
            , _sContentsHash = ch
            , _sAssignmentHash = aHash
            }

    return NewSubmission
        { nsAssignmentHash = aHash
        , nsContentsHash   = hash "mkay"
        , nsWitness        = SubmissionWitness
            { _swKey = toPublic student
            , _swSig = sign student (hash sub)
            }
        }

sendSubmission :: StudentApiClientNoAuth -> NewSubmission -> IO SubmissionStudentInfo
sendSubmission sc sub = do
    sAddSubmission sc sub

getAllCourses :: StudentApiClientNoAuth -> IO [Course]
getAllCourses sc = do
    map ciId <$> sGetCourses sc False def def def


getFairCV :: IO FairCV
getFairCV = generate arbitrary

checkFairCV :: WitnessClient -> FairCV -> IO FairCVCheckResult
checkFairCV wc fairCV =
    wCheckFairCV wc fairCV

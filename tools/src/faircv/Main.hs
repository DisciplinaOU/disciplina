import Data.Aeson
import Servant.Client
import Test.QuickCheck
import Time.Units (threadDelay, sec)

import Loot.Log

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Student
import Dscp.Educator.Web.Types
import Dscp.Resource.Keys
import Dscp.Util (leftToFail, nothingToFail)
import Dscp.Util.Test (detGenG)
import Dscp.Witness.Web hiding (checkFairCV)

import FaircvClient
import FaircvOptions

createActorOf :: SecretKey -> IO (PublicKey, Address)
createActorOf sk = do
    pk   <- return (toPublic sk)
    addr <- return (mkAddr   pk)
    return (pk, addr)

instance MonadLogging IO where
    log = putStrLn . fmtMessageColored

    logName = return CallstackName

main :: IO ()
main = do
    FaircvOptions {..} <- getFaircvOptions

    -- create the APIs clients
    wClient <- createWitnessClient =<< parseBaseUrl witnessUrl
    sClient <- createStudentApiClient =<< parseBaseUrl educatorUrl

    -- read educator's secret key from the file
    store <- readStore secretKeyFile emptyPassPhrase
    sk <- return $ store^.krSecretKey

    -- make student secret key from a seed
    skS <- nothingToFail "Could not make student secret key" $
        secretFromSeed studentSeed

    -- make keys and addresses for educator and students
    (_pk,  addr) <- createActorOf sk
    (pkS, addrS) <- createActorOf skS

    let seed = "47295" :: Text -- NOTE: this can be randomized, or user defined
        waitingTime = sec refreshRate
        infoToSubmission :: AssignmentStudentInfo -> Submission
        infoToSubmission assInfo = Submission
            { _sStudentId      = addrS
            , _sContentsHash   = detGenG seed arbitrary
            , _sAssignmentHash = aiHash assInfo
            }
        toSignedSubmission :: Submission -> SignedSubmission
        toSignedSubmission sbm = SignedSubmission
            { _ssSubmission = sbm
            , _ssWitness    = SubmissionWitness 
                { _swKey = pkS
                , _swSig = sign skS $ hash sbm
                }
            }
        infoToNewSub :: AssignmentStudentInfo -> NewSubmission
        infoToNewSub = signedSubmissionToRequest . toSignedSubmission . infoToSubmission
        waitForProofs :: [SubmissionStudentInfo] -> IO ()
        waitForProofs [] = return ()
        waitForProofs (info:infos) = do
            newInfo <- sGetSubmission sClient $ siHash info
            case giHasProof <$> siGrade newInfo of
                Just True -> waitForProofs infos
                _ -> threadDelay waitingTime >> waitForProofs (info:infos)

    -- Get all the available assignments for this student
    assLst <- sGetAssignments sClient Nothing Nothing Nothing False
    -- Make a new submission from the first one, then send it
    subInfos <- mapM (sendSubmission sClient . infoToNewSub) $ take 1 assLst

    -- Wait for the proofs to be available, then get them and make a faircv
    logInfo "Submissions sent, waiting for proofs"
    waitForProofs subInfos
    proofs <- sGetProofs sClient Nothing False
    
    fcvrs <- mapM (blkToFairCV addrS "John Doe" addr) proofs
    fcv <- unReadyFairCV <$> leftToFail (mergeFairCVList fcvrs)

    writeFile "fairCV-example.json" $ decodeUtf8 $ encode fcv

    print =<< checkFairCV wClient fcv

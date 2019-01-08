
import Data.Aeson
import Servant.Client
import Test.QuickCheck
import Time.Units (threadDelay, sec)

import Loot.Log

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Student
import Dscp.Resource.Keys
import Dscp.Util (leftToFail)
import Dscp.Util.Test (detGenG)
import Dscp.Witness.Web hiding (checkFairCV)

import Client

-- This is a copy of dscp-keygen's parseInputWithSecret (for SecretFromSeed)
secretFromSeed :: ByteString -> Maybe SecretKey
secretFromSeed input = asum [
      do
        sd <- readMaybe @Word . toString @Text $ decodeUtf8 input
        return $ withIntSeed (fromIntegral sd) genSecretKey
    , do
        return $ withSeed input genSecretKey
    ]

createActor :: IO (SecretKey, PublicKey, Address)
createActor = do
    Just sk   <- return $ secretFromSeed ("456" :: ByteString)
    pk   <- return (toPublic sk)
    addr <- return (mkAddr   pk)
    return (sk, pk, addr)

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
    [wUrl, eUrl, skFile] <- getArgs
    -- create the APIs clients
    wClient <- createWitnessClient =<< parseBaseUrl wUrl
    sClient <- createStudentApiClient =<< parseBaseUrl eUrl

    -- read educator's secret key from the file
    store <- readStore skFile emptyPassPhrase
    sk <- return $ store^.krSecretKey

    -- make keys and addresses for educator and students
    (_pk,  addr)      <- createActorOf sk
    (skS, pkS, addrS) <- createActor

    let seed = "47295" :: Text -- NOTE: this can be randomized, or user defined
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

    -- Get all the available assignments for this student
    assLst <- sGetAssignments sClient Nothing Nothing Nothing False
    -- Make a new submission from the first one, then send it
    mapM_ (sendSubmission sClient . infoToNewSub) $ take 1 assLst

    -- Wait some time for the proofs to be available, then get them and make a faircv
    logInfo "Submissions sent, waiting for proofs"
    threadDelay $ sec 20
    proofs <- sGetProofs sClient Nothing False
    
    fcvrs <- mapM (blkToFairCV addrS "John Doe" addr) proofs
    fcv <- unReadyFairCV <$> leftToFail (mergeFairCVList fcvrs)

    writeFile "fairCV-example.json" $ decodeUtf8 $ encode fcv

    print =<< checkFairCV wClient fcv

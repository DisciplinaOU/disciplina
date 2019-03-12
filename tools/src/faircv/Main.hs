import Data.Aeson
import Fmt ((+|), (|+))
import Servant.Client
import Servant.Util (fullContent, noFilters, noSorting)
import Test.QuickCheck
import Time.Units (threadDelay)

import Loot.Log

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Logic.Submission
import Dscp.Educator.Web.Student
import Dscp.Resource.Keys
import Dscp.Util (leftToFail)
import Dscp.Util.Test (detGenG)
import Dscp.Witness.Web hiding (checkFairCV)

import FaircvClient
import FaircvOptions

instance MonadLogging IO where
    log = putStrLn . fmtMessageColored

    logName = return CallstackName

main :: IO ()
main = do
    FaircvOptions {..} <- getFaircvOptions

    -- create the APIs clients
    wClient <- createWitnessClient =<< parseBaseUrl witnessUrl
    sClientUnauth <- createStudentApiClient =<< parseBaseUrl educatorUrl

    -- read educator's secret key from the file
    store <- readStore secretKeyFile emptyPassPhrase
    let sk = store^.krSecretKeyData

    -- make student secret key from a seed
    let skS = mkSecretKeyData $ secretFromSeed studentSeed

    -- make addresses for educator and students
    let addr = skAddress sk
        addrS = skAddress skS

    let sClient = sClientUnauth (Just skS)

    let infoToSignedSubmission :: AssignmentStudentInfo -> SignedSubmission
        infoToSignedSubmission assInfo =
            makeSignedSubmission skS (aiHash assInfo) (detGenG contentSeed arbitrary)

        infoToNewSub :: AssignmentStudentInfo -> NewSubmission
        infoToNewSub = signedSubmissionToRequest . infoToSignedSubmission
        waitForProofs :: [SubmissionStudentInfo] -> IO ()
        waitForProofs [] = return ()
        waitForProofs (info:infos) = do
            newInfo <- sGetSubmission sClient $ siHash info
            case giHasProof <$> siGrade newInfo of
                Just True -> waitForProofs infos
                _         -> threadDelay refreshRate >> waitForProofs (info:infos)

    -- Get all the available assignments for this student
    assLst <- sGetAssignments sClient False noFilters noSorting fullContent
    -- Show a warning when there are not enough assignments available
    let assNum = length assLst
    when (assignmentNum > assNum) $ logWarning $
        "Not enough assignments available, only " +|assNum|+ " will be used"
    -- Make new submissions from the assignments
    subInfos <- mapM (sendSubmission sClient . infoToNewSub) $ take assignmentNum assLst

    -- Wait for the proofs to be available, then get them and make a faircv
    logInfo "Submissions sent, waiting for proofs"
    waitForProofs subInfos
    proofs <- sGetProofs sClient False noFilters

    fcvrs <- mapM (blkToFairCV addrS "John Doe" addr) proofs
    fcv <- unReadyFairCV <$> leftToFail (mergeFairCVList fcvrs)

    -- Write the result in the specified file and show the validation result
    logInfo "Writing result to file"
    writeFile outputFile $ decodeUtf8 $ encode fcv

    checkFairCV wClient fcv >>= \res ->
        if fairCVFullyValid res
        then logInfo "FairCV was successfully validated"
        else logError "FairCV was not validated successfully"

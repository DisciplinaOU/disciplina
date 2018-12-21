
import Data.Aeson
import qualified Data.Set as Set
import Data.Traversable
import Data.Time.Clock
import Servant.Client
import Test.QuickCheck

import Loot.Log

import Dscp.Core
import Dscp.Crypto
import Dscp.Witness.Web hiding (checkFairCV)
import Dscp.Resource.Keys

import Client

createActor :: IO (SecretKey, PublicKey, Address)
createActor = do
    sk   <- genSecretKey
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
    [url, skFile] <- getArgs

    wClient <- createWitnessClient =<< parseBaseUrl url

    store <- readStore skFile emptyPassPhrase

    sk <- return $ store^.krSecretKey

    (pk,  addr)       <- createActorOf sk
    (skS, pkS, addrS) <- createActor

    privateTxs <- for [1..10 :: Integer] $ \_ -> do
        timestamp      <- getCurrentTime
        contentsHash   <- generate arbitrary
        assignmentHash <- generate arbitrary

        sub <- return Submission
            { _sStudentId      = addrS
            , _sContentsHash   = contentsHash
            , _sAssignmentHash = assignmentHash
            }

        return PrivateTx
            { _ptGrade            = gA
            , _ptTime             = timestamp
            , _ptSignedSubmission = SignedSubmission
                { _ssSubmission = sub
                , _ssWitness    = SubmissionWitness
                    { _swKey = pkS
                    , _swSig = sign skS (hash sub)
                    }
                }
            }

    tree <- return $ fromFoldable privateTxs
    sig  <- return $ getMerkleRoot tree

    hdr <- return PrivateBlockHeader
        { _pbhPrevBlock = genesisHeaderHash addr
        , _pbhBodyProof = sig
        , _pbhAtgDelta  = mempty
        }

    ptx <- return PublicationTx
        { ptAuthor     = addr
        , ptFeesAmount = unsafeMkCoin (100 :: Integer)
        , ptHeader     = hdr
        }

    res <- wSubmitPublication wClient PublicationTxWitnessed
        { ptwTx = ptx
        , ptwWitness = PublicationTxWitness
            { pwSig = sign sk (toPtxId ptx, pk, hdr)
            , pwPk  = pk
            }
        }

    print res

    Just proof <- return $ mkMerkleProof tree (Set.fromList [0..9])

    fcv :: FairCV <- return $ unReadyFairCV $ singletonFCV addr (hash hdr) (readyProof proof)

    writeFile "fairCV-example.json" $ decodeUtf8 $ encode fcv

    print =<< checkFairCV wClient fcv

    -- sClient <- createStudentApiClient =<< parseBaseUrl "127.0.0.1:8090"
    -- asses   <- getAssignments sClient
    -- print asses

module Dscp.Core.Test
    ( mkTestPrivateTx
    , mkPubKey
    , mkPrivKey
    , mkKeyPair
    ) where

import Data.Time.Format (defaultTimeLocale, parseTimeOrError)

import Dscp.Core.Foundation
import Dscp.Crypto
import Dscp.Util

-- | Create a private transaction
mkTestPrivateTx :: Id Course -- ^ course id
            -> Grade -- ^ grade
            -> PublicKey -- ^ public key to derive address from
            -> (PublicKey, SecretKey) -- ^ witness key pair
            -> PrivateTx
mkTestPrivateTx courseId grade addrKey (witnessPKey, witnessSKey) =
    PrivateTxGrade $
        PrivateGrade
            { _ptSignedSubmission = mkSignedSubmission
            , _ptGrade = grade
            , _ptTime = time
            }
  where
     time :: Timestamp
     time =
       toTimestamp $
          parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2018-03-04"

     mkSignedSubmission :: SignedSubmission
     mkSignedSubmission = SignedSubmission
       { _ssSubmission = mkSubmission
       , _ssWitness = mkSubmissionWitness
       }

     mkSubmission :: Submission
     mkSubmission = Submission
       { _sStudentId = mkAddr addrKey
       , _sContentsHash = offlineHash
       , _sAssignmentHash = hash mkAssignment
       }

     mkSubmissionWitness :: SubmissionWitness
     mkSubmissionWitness = SubmissionWitness
       { _swKey = witnessPKey
       , _swSig = sign witnessSKey (hash mkSubmission)
       }

     mkAssignment :: Assignment
     mkAssignment = Assignment
       { _aCourseId = courseId
       , _aContentsHash = offlineHash
       , _aType = Regular
       , _aDesc = ""
       }

----------------------------------------------------------------------------
-- Helpers for data generation
----------------------------------------------------------------------------

-- | Create public key from seed
mkPubKey :: Char -> PublicKey
mkPubKey seed = fst (mkKeyPair seed)

-- | Create private key from seed
mkPrivKey :: Char -> SecretKey
mkPrivKey seed = snd (mkKeyPair seed)

-- | Create key pair from seed
mkKeyPair :: Char -> (PublicKey, SecretKey)
mkKeyPair seed = swap $ withIntSeed (fromIntegral $ ord seed) keyGen

-- | Common educator-related utilities.

module Test.Dscp.Educator.Common where


import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)

import Dscp.Core
import Dscp.Crypto (PublicKey, SecretKey, hash, sign)
import Dscp.Educator (PrivateTx (..))
import Dscp.Util (Id)


-- | Create a private transaction
mkPrivateTx :: Id Course -- ^ course id
            -> Grade -- ^ grade
            -> PublicKey -- ^ public key to derive address from
            -> (PublicKey, SecretKey) -- ^ witness key pair
            -> PrivateTx
mkPrivateTx courseId grade addrKey (witnessPKey, witnessSKey) =
    PrivateTx { _ptSignedSubmission = mkSignedSubmission
              , _ptGrade = grade
              , _ptTime = time
              }
  where
     time :: UTCTime
     time = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2018-03-04"

     mkSignedSubmission :: SignedSubmission
     mkSignedSubmission = SignedSubmission
       { _ssSubmission = mkSubmission
       , _ssWitness = mkSubmissionWitness
       }

     mkSubmission :: Submission
     mkSubmission = Submission
       { _sStudentId = mkAddr addrKey
       , _sContentsHash = offlineHash
       , _sAssignment = mkAssignment
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

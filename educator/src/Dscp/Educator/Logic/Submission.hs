module Dscp.Educator.Logic.Submission
    ( signSubmission
    , makeSignedSubmission
    ) where

import Dscp.Core
import Dscp.Crypto

signSubmission :: SecretKeyData -> Submission -> SignedSubmission
signSubmission sk submission =
    SignedSubmission
    { _ssSubmission = submission
    , _ssWitness = SubmissionWitness
        { _swKey = skPublic sk
        , _swSig = sign (skSecret sk) (hash submission)
        }
    }

makeSignedSubmission
    :: SecretKeyData
    -> Hash Assignment
    -> Hash Raw
    -> SignedSubmission
makeSignedSubmission sk assignmentHash contentsHash =
    let submission =
            Submission
            { _sStudentId = skAddress sk
            , _sAssignmentHash = assignmentHash
            , _sContentsHash = contentsHash
            }
    in signSubmission sk submission

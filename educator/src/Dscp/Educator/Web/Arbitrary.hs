-- | Generators and examples for web types.

module Dscp.Educator.Web.Arbitrary
    ( gradeInfoEx
    , blkProofInfoEx
    ) where

import Universum
import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Types
import Dscp.Util.Aeson
import Dscp.Util.Test

gradeInfoEx :: GradeInfo
gradeInfoEx =
    GradeInfo
    { giSubmissionHash = hash submissionEx
    , giGrade = gradeEx
    , giTimestamp = timestampEx
    , giHasProof = True
    }

blkProofInfoEx :: BlkProofInfo
blkProofInfoEx =
    BlkProofInfo
    { bpiBlockHash = detGen 123 arbitrary
    , bpiMtreeSerialized = EncodeSerialised $ detGen 123 arbitrary
    , bpiTxs = one privateTxEx
    }

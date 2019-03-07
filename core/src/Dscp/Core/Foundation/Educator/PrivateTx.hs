
module Dscp.Core.Foundation.Educator.PrivateTx where

import Control.Lens (makeLenses, makePrisms)

import Dscp.Core.Foundation.Educator.Student
import Dscp.Core.Foundation.Educator.Certificate
import Dscp.Core.Foundation.Educator.Submission
import Dscp.Core.Foundation.Educator.Grade
import Dscp.Core.Foundation.Educator.Timestamp
import Dscp.Crypto

data PrivateTx
    = PrivateTxGrade !PrivateGrade
    | PrivateTxCertification !(Signed CertificateFullInfo)
      deriving (Show, Eq, Ord, Generic)

getPrivateTxType :: PrivateTx -> Int
getPrivateTxType = \case
    PrivateTxGrade         {} -> ptxTypeGrade
    PrivateTxCertification {} -> ptxTypeCertification

ptxTypeGrade, ptxTypeCertification :: Int
ptxTypeGrade = 0
ptxTypeCertification = 1

data PrivateCertification = PrivateCertification
    { _pcGrade   :: !SignedCertificateGrade
    , _pcStudent :: !Student
    } deriving (Show, Eq, Ord, Generic)

data SignedCertificateGrade = SignedCertificateGrade
    { _scgCertificateGrade :: !CertificateGrade
    , _scgKey              :: !PublicKey
    , _scgSig              :: !(Signature (Hash CertificateGrade))
    } deriving (Show, Eq, Ord, Generic)

-- | Private transaction.
data PrivateGrade = PrivateGrade
    { _ptSignedSubmission :: !SignedSubmission
    -- ^ Every transaction contains one signed student submission
    , _ptGrade            :: !Grade
    -- ^ Grade for this submission
    , _ptTime             :: !Timestamp
    -- ^ Timestamp for this transaction
    } deriving (Show, Eq, Ord, Generic)

type PrivateTxId = Hash PrivateTx

-- | Which data to sign in transaction.
-- 'PrivateTxId' is basically a hash of all transaction contents,
-- so it's sufficient to sign only that.
type PrivateTxSigData = PrivateTxId

-- | Type alias for private tx signature.
type PrivateTxSig = Signature PrivateTxSigData

-- | Witness contains data required to verify transaction.
-- Included 'PublicKey' belongs either to Student or Educator.
-- TODO: maybe we can say that Educator's key is already known
-- to everybody, and not include it into Educator's witness?
data PrivateTxWitness = PkWitness
    { _ptwKey :: !PublicKey
    , _ptwSig :: !PrivateTxSig
    } deriving (Show, Eq, Generic)

-- | Datatype for verifiable transaction (transaction with a witness)
data PrivateTxAux = PrivateTxAux
    { _ptaTx      :: !PrivateTx
    , _ptaWitness :: !PrivateTxWitness
    } deriving (Show, Eq, Generic)

makePrisms ''PrivateTx
makeLenses ''PrivateGrade
makeLenses ''SignedCertificateGrade
makeLenses ''PrivateCertification
makeLenses ''PrivateTxWitness
makeLenses ''PrivateTxAux


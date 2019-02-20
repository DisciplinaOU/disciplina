-- | Private-chain (educator) datatypes definition.

{-# LANGUAGE NumDecimals #-}

module Dscp.Core.Foundation.Educator
    (

    -- * Common types
      ItemDesc (..)
    , isValidItemDesc
    , toItemDesc
    , toItemDescUnsafe
    , Timestamp (..)
    , toTimestamp
    , toTimestampUnsafe
    , Course (..)
    , Subject (..)
    , Student
    , Grade (..)
    , mkGrade
    , Language (..)
    , gradeToNum
    , Assignment (..)
    , AssignmentType (..)
    , Submission (..)
    , offlineHash
    , DocumentType (..)
    , SubmissionSig
    , SignedSubmission (..)
    , SubmissionWitness (..)
    , CertificateFullInfo (..)
    , CertificateMeta (..)
    , CertificateGrade (..)
    , CertificateIssuerInfo (..)
    , SignedCertificateGrade (..)
    , Certificate (..)
    , CertificateName (..)
    , StudentInfo (..)
    , GradeInfo (..)
    , EducationForm (..)
    , GradingScale (..)
    , documentType
    , _aDocumentType
    , aDocumentType
    , aCourseId
    , aContentsHash
    , aType
    , aDesc
    , _sDocumentType
    , sDocumentType
    , sStudentId
    , sContentsHash
    , sAssignmentHash
    , swKey
    , swSig
    , ssSubmission
    , ssWitness

    -- * Private transactions
    , PrivateTx (..)
    , PrivateGrade (..)
    , PrivateCertification (..)
    , PrivateTxWitness (..)
    , PrivateTxAux (..)
    , _PrivateTxGrade
    , _PrivateTxCertification
    , getPrivateTxType
    , ptxTypeGrade
    , ptxTypeCertification

    -- * Lenses
    , ptaTx
    , ptaWitness
    , ptGrade
    , ptSignedSubmission
    , ptTime
    , ptwKey
    , ptwSig
    , pcGrade
    , pcStudent
    , scgKey
    , scgSig
    , scgCertificateGrade

    -- * Basic types
    , PrivateHeaderHash
    , PrivateBlockHeader (..)
    , pbhPrevBlock
    , pbhBodyProof
    , pbhAtgDelta
    , PrivateBlockBody (..)
    , pbbTxs
    , PrivateBlock (..)
    , pbHeader
    , pbBody

      -- * Constants
    , genesisHeaderHash

      -- * Helpers
    , getPrevBlockRefMaybe

    -- * Activity Type Graph
    , ATGSubjectChange (..)
    , ATGDelta (..)
    , isEmptyATGDelta
    , ATGNode (..)
    , atgnSubjectId
    , atgnChildren
    , ATGEdge (..)
    , atgeWeight
    , atgeChild
    , ATG (..)
    ) where

import Dscp.Core.Foundation.Educator.Assignment as M
import Dscp.Core.Foundation.Educator.ATGDelta as M
import Dscp.Core.Foundation.Educator.Certificate as M
import Dscp.Core.Foundation.Educator.Course as M
import Dscp.Core.Foundation.Educator.DocumentType as M
import Dscp.Core.Foundation.Educator.Educator as M
import Dscp.Core.Foundation.Educator.Grade as M
import Dscp.Core.Foundation.Educator.ItemDesc as M
import Dscp.Core.Foundation.Educator.Orphans ()
import Dscp.Core.Foundation.Educator.Student as M
import Dscp.Core.Foundation.Educator.Subject as M
import Dscp.Core.Foundation.Educator.Submission as M
import Dscp.Core.Foundation.Educator.Timestamp as M

import Control.Lens (Getter, makeLenses, makePrisms, to)
import qualified Data.ByteArray as BA

import Dscp.Core.Foundation.Address (Address (..))
import Dscp.Crypto
import Dscp.Util

-- | Datatype containing information about Educator which issued
-- the certificate, required in order to render a certificate.
data CertificateIssuerInfo = CertificateIssuerInfo
    { ciiName    :: ItemDesc
    , ciiWebsite :: ItemDesc
    , ciiId      :: Text
    } deriving (Show, Eq, Generic)

-- | Datatype which is used for encoding a full certificate ID.
data CertificateName = CertificateName
    { cnEducatorId    :: Text
    , cnCertificateId :: Hash CertificateMeta
    } deriving (Show, Eq, Generic)

instance Buildable (GradeInfo) where
    build (GradeInfo{..}) =
      "{ submission hash = " +| giSubmissionHash |+
      ", grade = " +| giGrade |+
      ", timestamp = " +| giTimestamp |+
      ", has proof = " +| giHasProof |+
      " }"

instance Buildable Certificate where
    build Certificate {..} =
        "{ id = "+|cId|+", meta = "+|cMeta|+" }"

----------------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------------

data PrivateTx
    = PrivateTxGrade !PrivateGrade
    | PrivateTxCertification !PrivateCertification
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

----------------------------------------------------------
-- Block elements
----------------------------------------------------------

-- | Hash of the private block.
type PrivateHeaderHash = Hash PrivateBlockHeader

-- | Header of a private block. There's no signatures here, as it's private anyway.
-- During publishing, Educator will provide a signature as a part of transaction.
data PrivateBlockHeader = PrivateBlockHeader
    { _pbhPrevBlock :: !PrivateHeaderHash
    -- ^ Previous header in the chain
    , _pbhBodyProof :: !(MerkleSignature PrivateTx)
    -- ^ Body payload proof (for now - only root of sized Merkle tree
    -- over private transactions)
    , _pbhAtgDelta  :: !ATGDelta
    -- ^ Changes in courses taught by Educator
    } deriving (Show, Eq, Ord, Generic)

makeLenses ''PrivateBlockHeader

instance Buildable PrivateBlockHeader where
    build PrivateBlockHeader {..} =
        "PrivateBlockHeader { prev: " +| _pbhPrevBlock |+
        "; body proof: " +| _pbhBodyProof |+
        "; atg:" +| _pbhAtgDelta |+ " }"

-- | Genesis hash, serves as previous block reference for the first block.
-- Different for each educator.
-- TODO: move to 'Genesis' module when it is formed somehow. Also, should
-- private genesis hash actually make some sense?
genesisHeaderHash :: Address -> PrivateHeaderHash
genesisHeaderHash (Address addr) =
    unsafeHash ("pvaforever" <> BA.convert addr :: ByteString)

-- | Get previous block header, if previous block exists,
-- 'Nothing' otherwise.
getPrevBlockRefMaybe :: PrivateBlockHeader -> Address -> Maybe PrivateHeaderHash
getPrevBlockRefMaybe PrivateBlockHeader {..} address =
    if _pbhPrevBlock == genesisHeaderHash address
    then Nothing
    else Just _pbhPrevBlock

-- | Private block body. Contains only private transactions (for now).
-- TODO: should we also store inner Merkle nodes in some sort of cache,
-- to provide quick positions?
data PrivateBlockBody = PrivateBlockBody
    { _pbbTxs :: ![PrivateTx]
    } deriving (Show, Eq, Generic)

makeLenses ''PrivateBlockBody

-- | Private block consists (surprisingly) of header and body.
-- We won't define a private undo yet, because we're yet to define
-- the usecase for rollbacks in private chain.
data PrivateBlock = PrivateBlock
    { _pbHeader :: !PrivateBlockHeader
    , _pbBody   :: !PrivateBlockBody
    } deriving (Show, Eq, Generic)

makeLenses ''PrivateBlock

---------------------------------------------------------------------
-- Activity Type Graph
---------------------------------------------------------------------

{-

@flyingleafe: I implemented a version of ATG without "etc" vertices
because I don't quite see the reason for having them.

-}

-- | Activity Type Graph node. Implementation without "etc" vertices.
-- TODO: should we use 'Vector' for more efficient indexing? or we don't
-- care, because it should be in DB somehow anyway?
data ATGNode = ATGNode
    { _atgnSubjectId :: !(Id Subject)
    , _atgnChildren  :: ![ATGEdge]
    } deriving (Show, Eq, Generic)

-- | Edge pointing to the children node in ATG.
data ATGEdge = ATGEdge
    { _atgeWeight :: !Float
    , _atgeChild  :: !ATGNode
    } deriving (Show, Eq, Generic)

-- | Activity Type Graph itself is just a list of root nodes.
newtype ATG = ATG
    { getATGRoots :: [ATGNode]
    } deriving (Show, Eq, Generic)

makeLenses ''ATGNode
makeLenses ''ATGEdge
makeLenses ''ATG

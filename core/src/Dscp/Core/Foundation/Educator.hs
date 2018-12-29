-- | Private-chain (educator) datatypes definition.

module Dscp.Core.Foundation.Educator
    (

    -- * Common types
      Course (..)
    , Subject (..)
    , Student
    , Grade (..)
    , mkGrade
    , EducatorId
    , Assignment (..)
    , AssignmentType (..)
    , Submission (..)
    , offlineHash
    , DocumentType (..)
    , SubmissionSig
    , SignedSubmission (..)
    , SubmissionWitness (..)
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
    , _ptSubmission
    , PrivateTxWitness (..)
    , PrivateTxAux (..)

    -- * Lenses
    , ptaTx
    , ptaWitness
    , ptGrade
    , ptSignedSubmission
    , ptTime
    , ptwKey
    , ptwSig

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

import Control.Lens (Getter, makeLenses, to)
import qualified Data.ByteArray as BA
import Data.Time.Clock (UTCTime)
import Fmt (build, genericF, mapF, (+|), (|+))

import Dscp.Core.Foundation.Address (Address (..))
import Dscp.Crypto
import Dscp.Util (HasId (..))

----------------------------------------------------------------------------
-- Common educator types
----------------------------------------------------------------------------

-- | ID of particular subject.
newtype Subject = Subject
    { getSubjectId :: Word32
    } deriving (Eq, Ord, Show, Num)

instance Buildable Subject where
    build (Subject n) = build n

instance HasId Subject

-- | Assignment/course grade.
-- An integer from 0 to 100. Constructor is unsafe, because it's possible
-- to make a grade outside these bounds.
newtype Grade = UnsafeGrade
    { getGrade :: Word8
    } deriving (Eq, Ord, Show, Generic)

instance Bounded Grade where
    minBound = UnsafeGrade 0
    maxBound = UnsafeGrade 100

instance Buildable Grade where
    build = build . getGrade

mkGrade :: Word8 -> Maybe Grade
mkGrade a =
    let g = UnsafeGrade a
    in g <$ guard (g >= minBound && g <= maxBound)

-- | Student is identified by their public address.
type Student = Address

instance HasId Student

-- | Educator is identified by their public adddress.
type EducatorId = Address

-- | Educator's course ID is simply a 'Word32' too.
-- There's a mapping from course ID to a set of associated subject IDs.
newtype Course = Course
    { getCourseId :: Word32
    } deriving (Eq, Ord, Show, Num)

instance HasId Course

-- | Assignment can be either regular of final
data AssignmentType = Regular | CourseFinal
    deriving (Eq, Ord, Show, Enum, Generic)

-- | Assignment doesn't contain actual assignment contents - only hash of them.
data Assignment = Assignment
    { _aCourseId     :: !(Id Course)
    -- ^ Course this assignement belongs to
    , _aContentsHash :: !(Hash Raw)
    -- ^ Hash of assignment contents
    , _aType         :: !AssignmentType
    -- ^ Assignment type
    , _aDesc         :: !Text
    -- ^ Description of assignment
    } deriving (Eq, Ord, Show, Generic)

-- | We cannot make it do 'hash' on 'Assignment' direclty, because 'Serialisable' instance
--   is required for that. And when we `import Dscp.Educator.Serialise ()` we get dependency
--   loop.
--
--   That's why we do "late binding" here.
instance HasHash Assignment => HasId Assignment where
    type Id Assignment = Hash Assignment
    getId = hash

-- | Student submissions
data Submission = Submission
    { _sStudentId      :: !(Id Student)
    -- ^ Student who created this submission
    , _sContentsHash   :: !(Hash Raw)
    -- ^ Hash of submission contents
    , _sAssignmentHash :: !(Hash Assignment)
    -- ^ Assignment of this submission
    } deriving (Eq, Ord, Show, Generic)

-- | A hash which indicates that a submission or an assignment
-- are offline.
-- TODO: make a more comprehensible and easily documentable value?...
offlineHash :: Hash Raw
offlineHash = unsafeHash ("offline" :: ByteString)

-- | Datatype to represent the notion of "offline"- and "online"-ness
-- of assignments and submissions.
data DocumentType = Online | Offline
    deriving (Eq, Ord, Show, Enum, Generic)

documentType :: Hash Raw -> DocumentType
documentType h
    | h == offlineHash = Offline
    | otherwise        = Online

_aDocumentType :: Assignment -> DocumentType
_aDocumentType = documentType . _aContentsHash

aDocumentType :: Getter Assignment DocumentType
aDocumentType = to _aDocumentType

_sDocumentType :: Submission -> DocumentType
_sDocumentType = documentType . _sContentsHash

sDocumentType :: Getter Submission DocumentType
sDocumentType = to _sDocumentType

instance HasHash Submission => HasId Submission where
    type Id Submission = Hash Submission
    getId = hash

-- | Type alias for Submission signature.
type SubmissionSig = Signature (Id Submission)

-- | Witness contains data required to verify transaction.
data SubmissionWitness = SubmissionWitness
    { _swKey :: !PublicKey
    , _swSig :: !SubmissionSig
    } deriving (Show, Eq, Ord, Generic)

-- | Datatype for verifiable transaction (transaction with a witness)
data SignedSubmission = SignedSubmission
    { _ssSubmission :: !Submission
    -- ^ Student submission
    , _ssWitness    :: !SubmissionWitness
    -- ^ Submission witness
    } deriving (Eq, Ord, Show, Generic)

instance HasHash Submission => HasId SignedSubmission where
    type Id SignedSubmission = Hash Submission
    getId = hash . _ssSubmission

makeLenses ''Assignment
makeLenses ''Submission
makeLenses ''SubmissionWitness
makeLenses ''SignedSubmission

data ATGSubjectChange
    = ATGAdded
    | ATGRemoved
    deriving (Eq, Ord, Show, Generic)

instance Buildable ATGSubjectChange where
    build = \case
        ATGAdded -> "added"
        ATGRemoved -> "removed"

-- | ATGDelta is a diff for set of subjects which are taught by Educator.
-- Implemented as 'Map SubjectId Bool' to avoid representing invalid diffs
-- (like, subject is present simultaneously in added and removed sets).
-- TODO: maybe we should separately make up a library for such stuff,
-- like 'MapModifier'?
newtype ATGDelta = ATGDelta
    { getATGDelta :: Map (Id Subject) ATGSubjectChange
    } deriving (Show, Eq, Ord, Monoid, Generic)

instance Buildable ATGDelta where
    build (ATGDelta d) = "ATGDelta { " +| mapF d |+ " }"

-- | Whether does 'ATGDelta' carries no changes actually.
isEmptyATGDelta :: ATGDelta -> Bool
isEmptyATGDelta  = null . getATGDelta

instance Buildable Course where
    build Course{..} = build getCourseId

instance Buildable () where
    build _ = ""

instance Buildable DocumentType where
    build = genericF

----------------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------------

-- | Private transaction.
data PrivateTx = PrivateTx
    { _ptSignedSubmission :: !SignedSubmission
    -- ^ Every transaction contains one signed student submission
    , _ptGrade            :: !Grade
    -- ^ Grade for this submission
    , _ptTime             :: !UTCTime
    -- ^ Timestamp for this transaction
    } deriving (Show, Eq, Ord, Generic)

_ptSubmission :: PrivateTx -> Submission
_ptSubmission = _ssSubmission . _ptSignedSubmission

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

makeLenses ''PrivateTx
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

-- | Core types used across Disciplina codebase.

module Dscp.Core.Types
       (
       -- * Addresses
         Address (..)
       , mkAddr
       , StakeholderId (..)

       -- * Private chain

       , Course (..)
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
       , sAssignment
       , swKey
       , swSig
       , ssSubmission
       , ssWitness

       -- * Activity Type Graph
       , ATGDelta (..)
       , ATGNode (..)
       , atgnSubjectId
       , atgnChildren
       , ATGEdge (..)
       , atgeWeight
       , atgeChild
       , ATG (..)

       -- * Blocks/txs
       , Coin (..)
       , TxElem (..)
       , Tx (..)
       , TxId
       , TxWitness (..)
       , TxWithWitness (..)
       ) where

import Control.Lens (Getter, makeLenses, to)
import Data.Map (Map)
import qualified Data.Text.Buildable
import Fmt (listF, (+||), (||+))

import qualified Snowdrop.Model.State.Accounting.Account as SD

import Dscp.Crypto (HasHash, Hash, PublicKey, Raw, Signature, hash, unsafeHash)
import Dscp.Util (HasId (..))

----------------------------------------------------------------------------
-- General
----------------------------------------------------------------------------

-- | 'Address' datatype. Not 'newtype', because later it will
-- inevitably become more complex.
-- TODO: maybe we should use a shorter hash for address, like in Cardano?
data Address = Address
    { addrHash :: !(Hash PublicKey)
    } deriving (Eq, Ord, Show, Generic)

mkAddr :: PublicKey -> Address
mkAddr = Address . hash

newtype StakeholderId = StakeholderId
    { unStakeholderId :: PublicKey
    } deriving (Eq, Show, Generic)

----------------------------------------------------------------------------
-- Private chain
----------------------------------------------------------------------------

-- | ID of particular subject.
newtype Subject = Subject
    { getSubjectId :: Word32
    } deriving (Eq, Ord, Show, Num)

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

mkGrade :: Word8 -> Maybe Grade
mkGrade a
    | a >= minBound && a <= maxBound = Just $ UnsafeGrade a
    | otherwise                      = Nothing

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
    deriving (Eq, Ord, Show, Generic)

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
    { _sStudentId    :: !(Id Student)
    -- ^ Student who created this submission
    , _sContentsHash :: !(Hash Raw)
    -- ^ Hash of submission contents
    , _sAssignment   :: !Assignment
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
    } deriving (Show, Eq, Generic)

-- | Datatype for verifiable transaction (transaction with a witness)
data SignedSubmission = SignedSubmission
    { _ssSubmission :: !Submission
    -- ^ Student submission
    , _ssWitness    :: !SubmissionWitness
    -- ^ Submission witness
    } deriving (Eq, Show, Generic)

instance HasHash Submission => HasId SignedSubmission where
    type Id SignedSubmission = Hash Submission
    getId = hash . _ssSubmission

makeLenses ''Assignment
makeLenses ''Submission
makeLenses ''SubmissionWitness
makeLenses ''SignedSubmission

-- | ATGDelta is a diff for set of subjects which are taught by Educator.
-- Implemented as 'Map SubjectId Bool' to avoid representing invalid diffs
-- (like, subject is present simultaneously in added and removed sets).
-- TODO: maybe we should separately make up a library for such stuff,
-- like 'MapModifier'?
newtype ATGDelta = ATGDelta
    { getATGDelta :: Map (Id Subject) Bool
    } deriving (Show, Eq, Ord, Generic)

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

----------------------------------------------------------------------------
-- Blocks/Transaction
----------------------------------------------------------------------------

-- This is all naive for now, should be moved to the separate module later

-- | Coin amount.
newtype Coin = Coin Word64
    deriving (Eq, Ord, Show, Generic, Hashable, Num)

instance Buildable Coin where
    build (Coin c) = c ||+ " coin(s)"

-- | Transaction element, is used both in inputs and in outputs.
data TxElem = TxElem
    { txInFrom  :: SD.Account
    , txInValue :: Coin
    } deriving (Eq, Ord, Generic, Show)

instance Buildable TxElem where
    build TxElem{..} = "<" +|| txInFrom ||+ ", " +|| txInValue ||+ ">"

-- | Transaction. Accounting-style.
data Tx = Tx
    { txIns  :: [TxElem]
    , txOuts :: [TxElem]
    } deriving (Eq, Ord, Generic, Show)

instance Buildable Tx where
    build Tx{..} = "Tx { in: " +|| listF txIns ||+ "; outs:" +|| listF txOuts ||+ " }"

type TxId = Hash Tx

-- | Transaction witness.
newtype TxWitness = TxWitness
    { unTxWitness :: [Signature Tx]
    } deriving (Eq, Show, Generic)

instance Buildable TxWitness where
    build (TxWitness w) = "TxWitness { " +|| listF w ||+ " }"

-- | Transaction coupled with witness.
data TxWithWitness = TxWithWitness
    { twTx      :: Tx
    , twWitness :: TxWitness
    } deriving (Eq, Show, Generic)

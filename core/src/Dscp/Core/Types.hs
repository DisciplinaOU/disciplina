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

       -- * Transaction
       , Coin (..)
       , coinToInteger
       , coinFromInteger
       , TxInAcc (..)
       , TxOut (..)
       , Tx (..)
       , TxId
       , toTxId
       , TxWitness (..)
       , TxWitnessed (..)
       , GTx (..)
       , GTxWitnessed (..)

       -- * Block
       , HeaderHash
       , Difficulty (..)
       , BlockToSign (..)
       , Header (..)
       , Block (..)
       , BlockBody (..)
       ) where

import Codec.Serialise (Serialise)
import Control.Lens (Getter, makeLenses, to)
import Data.Map (Map)
import Fmt (blockListF, build, indentF, listF, nameF, (+|), (+||), (|+), (||+))

import Dscp.Core.Address
import Dscp.Crypto (HasHash, Hash, PublicKey, Raw, Signature, hash, hashF, unsafeHash)
import Dscp.Util (HasId (..))

----------------------------------------------------------------------------
-- General
----------------------------------------------------------------------------


newtype StakeholderId = StakeholderId
    { unStakeholderId :: PublicKey
    } deriving (Eq, Show, Generic)

-- This is all naive for now, should be moved to the separate module later

-- | Coin amount.
newtype Coin = Coin { unCoin :: Word64 }
    deriving (Eq, Ord, Show, Generic, Hashable, Bounded)

-- | Safely convert coin to integer.
coinToInteger :: Coin -> Integer
coinToInteger = toInteger . unCoin

coinFromInteger :: Integer -> Either Text Coin
coinFromInteger i
    | i < 0
        = Left "Negative coin amount"
    | i > fromIntegral (unCoin maxBound)
        = Left "Coin amount is too high"
    | otherwise
        = Right (Coin $ fromIntegral i)

instance Buildable Coin where
    build (Coin c) = c ||+ " coin(s)"

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
    } deriving (Show, Eq, Ord, Monoid, Generic)

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
-- Transactions
----------------------------------------------------------------------------

-- We have several different types of "transactions". Money
-- transactions, delegation transactions (in the future). Money
-- transactions are the most popular, so we'll call them just
-- "transactions".

-- | Tx input account. Can be used for other tx types too.
data TxInAcc = TxInAcc
    { tiaAddr  :: Address
    , tiaNonce :: Integer
    } deriving (Eq, Ord, Generic, Show)

instance Buildable TxInAcc where
    build TxInAcc{..} = "TxInnAcc {" +| tiaAddr |+ " nonce " +|| tiaNonce ||+ "}"

-- | Money transaction output.
data TxOut = TxOut
    { txOutAddr  :: Address
    , txOutValue :: Coin
    } deriving (Eq, Ord, Generic, Show)

instance Buildable TxOut where
    build TxOut{..} = "<" +| txOutAddr |+ ", " +| txOutValue |+ ">"

-- | Transaction. Accounting-style money transfer.
data Tx = Tx
    { txInAcc   :: TxInAcc
    , txInValue :: Coin
    , txOuts    :: [TxOut]
    } deriving (Eq, Ord, Generic, Show)

instance Buildable Tx where
    build Tx{..} =
        "Tx { from: " +| txInAcc |+ "; inValue: " +| txInValue |+ "; outs:" +| listF txOuts |+ " }"

type TxId = Hash Tx

-- | Compute tx id.
toTxId :: (Serialise Tx) => Tx -> TxId
toTxId = hash

-- | Transaction witness. We sign a pair of transaction hash and private
-- key. The second element is there to authenticate proposed changes
-- (@kirill.andreev). Public key hash should be equal to the input address.
-- Also, public key should be the same which used to validate signature.
data TxWitness = TxWitness
    { txwSig :: Signature (TxId, PublicKey)
    , txwPk  :: PublicKey
    } deriving (Eq, Show, Generic)

instance Buildable TxWitness where
    build TxWitness {..} =
        "TxWitness { " +| txwSig |+ ", pk: " +| txwPk |+ " }"

-- | Transaction coupled with witness.
data TxWitnessed = TxWitnessed
    { twTx      :: Tx
    , twWitness :: TxWitness
    } deriving (Eq, Show, Generic)

instance Buildable TxWitnessed where
    build TxWitnessed {..} =
        "TxWitnessed { " +| twTx |+ ", " +| twWitness |+  " }"


-- | Generalised version of transaction, other types to appear
-- here.
data GTx =
    GMoneyTx Tx
    deriving (Generic, Eq, Show)

instance Buildable GTx where
    build (GMoneyTx tw) = "GMoneyTx: " +| tw |+ ""

data GTxWitnessed =
    GMoneyTxWitnessed TxWitnessed
    deriving (Generic, Eq, Show)

instance Buildable GTxWitnessed where
    build (GMoneyTxWitnessed tw) = "GMoneyTxWitnessed: " +| tw |+ ""

----------------------------------------------------------------------------
-- Blocks/Transaction
----------------------------------------------------------------------------

newtype Difficulty = Difficulty Word64
    deriving (Eq,Ord,Num,Show,Generic,Buildable)

-- | Blocks are indexed by their headers' hashes.
type HeaderHash = Hash Header

-- Part of the block we sign
data BlockToSign =
    BlockToSign Difficulty HeaderHash BlockBody
    deriving (Eq, Show, Generic)

data Header = Header
    { hSignature  :: Signature BlockToSign
    , hIssuer     :: PublicKey
    , hDifficulty :: Difficulty
    , hPrevHash   :: HeaderHash
    } deriving (Eq, Show, Generic)

instance Buildable Header where
    build Header{..} =
        "Header: " +|
        indentF 2 (blockListF [ nameF "sig" $ build hSignature
                              , nameF "issuer" $ build hIssuer
                              , nameF "difficulty" $ build hDifficulty
                              , nameF "prev" $ hashF hPrevHash ])

-- | Body of the block.
data BlockBody = BlockBody
    { rbbTxs :: [GTxWitnessed]
    } deriving (Eq, Show, Generic)

instance Buildable BlockBody where
    build (BlockBody txs) = listF txs

-- | Block.
data Block = Block
    { rbHeader :: Header
    , rbBody   :: BlockBody
    } deriving (Eq, Show, Generic)

instance Buildable Block where
    build = build . (show :: Block -> Text)

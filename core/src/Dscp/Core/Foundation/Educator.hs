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

import Control.Exception as E
import Control.Lens (Getter, makeLenses, makePrisms, to)
import qualified Data.ByteArray as BA
import qualified Data.Text as T
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds, picosecondsToDiffTime)
import Fmt (build, genericF, listF, mapF, (+|), (|+))

import Dscp.Core.Foundation.Address (Address (..))
import Dscp.Crypto
import Dscp.Util

-- | Description of some object.
-- Cannot contain @\0@ symbol (Postgres truncates such strings for 'TEXT' type).
-- TODO [DSCP-416]: move to SQL utils moved to core.
newtype ItemDesc = ItemDescUnsafe { unItemDesc :: Text }
    deriving (Show, Eq, Ord, Buildable, Semigroup, Monoid)

isValidItemDesc :: Text -> Bool
isValidItemDesc = isNothing . T.find (== '\0')

toItemDesc :: Text -> Either Text ItemDesc
toItemDesc t
    | isValidItemDesc t = Right (ItemDescUnsafe t)
    | otherwise = Left "Text contains \0 characters."

toItemDescUnsafe :: Text -> ItemDesc
toItemDescUnsafe t = E.assert (isValidItemDesc t) (ItemDescUnsafe t)

instance IsString ItemDesc where
    fromString = leftToPanic . toItemDesc . fromString

-- | Timestamp with up to microseconds precision, as it is stored by Postgres.
-- Helps to avoid confusion like when a time value gets rounded on itself.
--
-- Since Postgres truncates not only timestamps it stores, but also the ones we compare on
-- in @SELECT@ query, this type should appear in web API as well.
-- TODO [DSCP-416]: move to SQL utils moved to core.
newtype Timestamp = TimestampUnsafe { unTimestamp :: UTCTime }
    deriving (Show, Eq, Ord, Buildable)

-- | Rounds time to microseconds.
toTimestamp :: UTCTime -> Timestamp
toTimestamp utc = TimestampUnsafe
    utc{ utctDayTime =
            picosecondsToDiffTime . roundPcsToMcs . diffTimeToPicoseconds $
            utctDayTime utc
       }
  where
    roundPcsToMcs = (* 1e6) . round . (/ 1e6) . fromIntegral @_ @Double

toTimestampUnsafe :: UTCTime -> Timestamp
toTimestampUnsafe = TimestampUnsafe

----------------------------------------------------------------------------
-- Common educator types
----------------------------------------------------------------------------

-- TODO [DSCP-416]: extract "Int64" to reasonable type helper.
-- | ID of particular subject.
newtype Subject = Subject
    { getSubjectId :: Int64
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

gradeToNum :: Num i => Grade -> i
gradeToNum (UnsafeGrade g) = fromIntegral g

-- | Student is identified by their public address.
type Student = Address

instance HasId Student

-- | Educator's course ID is simply a number too.
-- There's a mapping from course ID to a set of associated subject IDs.
newtype Course = Course
    { getCourseId :: Int64
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
    , _aDesc         :: !ItemDesc
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
data DocumentType a = Online | Offline
    deriving (Eq, Ord, Show, Enum, Generic)

documentType :: Hash Raw -> DocumentType a
documentType h
    | h == offlineHash = Offline
    | otherwise        = Online

_aDocumentType :: Assignment -> DocumentType Assignment
_aDocumentType = documentType . _aContentsHash

aDocumentType :: Getter Assignment (DocumentType Assignment)
aDocumentType = to _aDocumentType

_sDocumentType :: Submission -> DocumentType Submission
_sDocumentType = documentType . _sContentsHash

sDocumentType :: Getter Submission (DocumentType Submission)
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

instance Buildable (DocumentType a) where
    build = genericF

-- | Datatype containing info about a certificate issued by Educator.
data CertificateMeta = CertificateMeta
    { cmStudentName      :: !ItemDesc
    , cmStudentBirthDate :: !Day
    , cmStartYear        :: !Word16
    , cmEndYear          :: !Word16
    , cmEducationForm    :: !EducationForm
    , cmNumber           :: !Natural
    , cmIssueDate        :: !Day
    , cmTitle            :: !ItemDesc
    , cmMajor            :: !ItemDesc
    , cmSpecialization   :: !(Maybe ItemDesc)
    } deriving (Show, Eq, Generic)

data EducationForm = Fulltime | Parttime | Fullpart
    deriving (Show, Eq, Generic, Enum, Bounded)

data GradingScale = RusDiff | RusNonDiff
    deriving (Show, Eq, Generic, Enum, Bounded)

-- | Datatype which combines certificate meta with its ID.
data Certificate = Certificate
    { cId   :: Hash CertificateMeta
    , cMeta :: CertificateMeta
    } deriving (Show, Eq, Generic)

-- | Datatype which contains information about the grade which
-- gets included into the certificate.
data CertificateGrade = CertificateGrade
    { cgSubject :: ItemDesc
    , cgLang    :: Language
    , cgHours   :: Word32
    , cgCredits :: Maybe Word32
    , cgScale   :: GradingScale
    , cgGrade   :: Grade
    } deriving (Show, Eq, Ord, Generic)

data Language = EN | RU
    deriving (Show, Eq, Ord, Generic)

-- | Datatype which contains all the info about certificate. This
-- datatype represents a request body for 'AddCertificate' endpoint.
data CertificateFullInfo = CertificateFullInfo
    { cfiMeta   :: CertificateMeta
    , cfiGrades :: NonEmpty CertificateGrade
    } deriving (Show, Eq, Generic)

data StudentInfo = StudentInfo
    { siAddr :: Student
    } deriving (Show, Eq, Ord, Generic)

data GradeInfo = GradeInfo
    { giSubmissionHash :: (Hash Submission)
    , giGrade          :: Grade
    , giTimestamp      :: Timestamp
    , giHasProof       :: Bool
    } deriving (Show, Eq, Ord, Generic)

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

instance Buildable (StudentInfo) where
    build (StudentInfo{..}) =
      "{ address = " +| siAddr |+
      " }"

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

instance Buildable CertificateGrade where
    build CertificateGrade {..} =
        "{ subject = "+|cgSubject|+
        ", hours = "+|cgHours|+
        ", credits = "+|cgCredits|+
        ", grade = "+|cgGrade|+" }"

instance Buildable CertificateFullInfo where
    build CertificateFullInfo {..} =
        "{ meta = "+|cfiMeta|+
        ", grades = "+|listF cfiGrades|+" }"

instance Buildable EducationForm where
    build = show

instance Buildable CertificateMeta where
    build CertificateMeta {..} =
        "{ studentName = "+|cmStudentName|+
        ", studentBirthDate = "+|cmStudentBirthDate|+
        ", startYear = "+|cmStartYear|+
        ", endYear = "+|cmEndYear|+
        ", educationForm = "+|cmEducationForm|+
        ", number = "+|toInteger cmNumber|+
        ", issueDate = "+|cmIssueDate|+
        ", title = "+|cmTitle|+
        ", major = "+|cmMajor|+
        ", specialization = "+|cmSpecialization|+" }"

instance Buildable CertificateName where
    build (CertificateName eId cId) =
        "certificate { educator-id = "+|eId|+", hash = "+|build cId|+"}"


----------------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------------

data PrivateTx
    = PrivateTxGrade !PrivateGrade
    | PrivateTxCertification !PrivateCertification
      deriving (Show, Eq, Ord, Generic)

data PrivateCertification = PrivateCertification
    { _pcGrade   :: !SignedCertificateGrade
    , _pcStudent :: !Student
    } deriving (Show, Eq, Ord, Generic)

data SignedCertificateGrade = SignedCertificateGrade
    { _scgCertificateGrade :: !CertificateGrade
    , _scgKey              :: !PublicKey
    , _scgSig              :: !(Signature CertificateGrade)
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


-- | Core types used across Disciplina codebase.

module Disciplina.Core.Types
       ( Address (..)
       , mkAddr
       , SubjectId (..)
       , Grade (..)
       , StudentId
       , EducatorId
       , CourseId (..)
       , AssignmentId
       , Submission

       -- * Activity Type Graph
       , ATGDelta (..)
       , ATGNode (..)
       , atgnSubjectId
       , atgnChildren
       , ATGEdge (..)
       , atgeWeight
       , atgeChild
       , ATG (..)
       ) where

import Universum

import Control.Lens (makeLenses)
import Data.Map (Map)

import Disciplina.Crypto (Hash, PublicKey, Raw, hash)

-- | 'Address' datatype. Not 'newtype', because later it will
-- inevitably become more complex.
-- TODO: maybe we should use a shorter hash for address, like in Cardano?
data Address = Address
    { addrHash :: !(Hash PublicKey)
    } deriving (Eq, Ord, Show, Generic)

mkAddr :: PublicKey -> Address
mkAddr = Address . hash

-- | ID of particular subject.
newtype SubjectId = SubjectId
    { getSubjectId :: Word32
    } deriving (Eq, Ord, Show, Num)

-- | Assignment/course grade.
-- TODO: decide on final format of the grade.
data Grade = F | D | C | B | A
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)

-- | Student is identified by their public address.
type StudentId = Address

-- | Educator is identified by their public adddress.
type EducatorId = Address

-- | Educator's course ID is simply a 'Word32' too.
-- There's a mapping from course ID to a set of associated subject IDs.
newtype CourseId = CourseId
    { getCourseId :: Word32
    } deriving (Eq, Ord, Show, Num)

-- | 'AssignmentId' is a hash of assignment contents,
-- which are stored off-chain.
data Assignment

type AssignmentId = Hash Assignment

-- | Stub type for submissions. Submission transaction
-- doesn't contain actual submission contents - only hash of them.
type Submission = Hash Raw

-- | ATGDelta is a diff for set of subjects which are taught by Educator.
-- Implemented as 'Map SubjectId Bool' to avoid representing invalid diffs
-- (like, subject is present simultaneously in added and removed sets).
-- TODO: maybe we should separately make up a library for such stuff,
-- like 'MapModifier'?
newtype ATGDelta = ATGDelta
    { getATGDelta :: Map SubjectId Bool
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
    { _atgnSubjectId :: !SubjectId
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

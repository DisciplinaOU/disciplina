
module Dscp.Core.Foundation.Educator.Assignment where

import Dscp.Core.Foundation.Educator.Course
import Dscp.Core.Foundation.Educator.ItemDesc
import Dscp.Crypto
import Dscp.Util

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

makeLenses ''Assignment

_aDocumentType :: Assignment -> DocumentType
_aDocumentType = documentType . _aContentsHash

aDocumentType :: Getter Assignment DocumentType
aDocumentType = to _aDocumentType

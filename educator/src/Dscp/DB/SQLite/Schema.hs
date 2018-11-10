{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Dscp.DB.SQLite.Schema
    ( module Dscp.DB.SQLite.Schema
    ) where

import Prelude hiding (_1, _2)

import Control.Lens (Field1 (..), Field2 (..), lens, makeLenses)
import Data.Time.Clock (UTCTime)
import Database.Beam.Schema.Tables (Beamable, C, Columnar, Database, DatabaseSettings, Table (..),
                                    TableEntity, defaultDbSettings)
import Database.SQLite.Simple.Internal (Connection (..))
import Database.SQLite3 (exec)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQLite.FileQuoter (qFile)
import Dscp.DB.SQLite.Types
import Dscp.Util

-- | Create tables if absent.
ensureSchemaIsSetUp :: MonadIO m => Connection -> m ()
ensureSchemaIsSetUp (Connection db) = do
    liftIO $ exec db schema

-- | Apply schema settings, should be invoked for every new connection.
applySchemaSettings :: MonadIO m => Connection -> m ()
applySchemaSettings (Connection db) =
    liftIO $ exec db schemaSettings

schema :: IsString s => s
schema = [qFile|./database/schema.sql|]

schemaSettings :: IsString s => s
schemaSettings = [qFile|./database/settings.sql|]

----------------------------------------------------------------------------
-- Tables
----------------------------------------------------------------------------

-- | Type of two entities relation.
data RelationType
    = Mx1  -- ^ Many-to-one
    | MxM  -- ^ Many-to-many

-- | Table which stores a relation between two tables.
-- TODO does beam really not provide something like this?
data RelationT (t :: RelationType) a b f = Columnar f a :-: Columnar f b
    deriving (Generic)

instance (rel ~ RelationT t a b f, ca ~ Columnar f a) => Field1 rel rel ca ca where
    _1 = lens (\(a :-: _) -> a) (\(_ :-: b) a -> (a :-: b))
instance (rel ~ RelationT t a b f, cb ~ Columnar f b) => Field2 rel rel cb cb where
    _2 = lens (\(_ :-: b) -> b) (\(a :-: _) b -> (a :-: b))


data CourseRowT f = CourseRow
    { _crId   :: C f Course
    , _crDesc :: C f Text
    } deriving (Generic)
makeLenses ''CourseRowT

data SubjectRowT f = SubjectRow
    { _srId     :: C f Subject
    , _srCourse :: C f Course
    , _srDesc   :: C f Text
    } deriving (Generic)
makeLenses ''SubjectRowT

data StudentRowT f = StudentRow
    { _srAddr     :: C f Address
    } deriving (Generic)
makeLenses ''StudentRowT

data AssignmentRowT f = AssignmentRow
    { _arHash         :: C f (Hash Assignment)
    , _arCourse       :: C f (Id Course)
    , _arContentsHash :: C f (Hash Raw)
    , _arType         :: C f AssignmentType
    , _arDesc         :: C f Text
    } deriving (Generic)
makeLenses ''AssignmentRowT

data SubmissionRowT f = SubmissionRow
    { _srHash           :: C f (Hash Submission)
    , _srStudent        :: C f Student
    , _srAssignmentHash :: C f (Hash Assignment)
    , _srContentsHash   :: C f (Hash Raw)
    , _srSignature      :: C f SubmissionWitness
    , _srCreationTime   :: C f UTCTime
    } deriving (Generic)
makeLenses ''SubmissionRowT

data TransactionRowT f = TransactionRow
    { _trHash           :: C f (Hash PrivateTx)
    , _trSubmissionHash :: C f (Hash Submission)
    , _trGrade          :: C f Grade
    , _trCreationTime   :: C f UTCTime
    , _trIdx            :: C f TxBlockIdx
    } deriving (Generic)
makeLenses ''TransactionRowT

-- We need `idx` field to be able to perform queries like "get N last blocks" efficiently.
data BlockRowT f = BlockRow
    { _brIdx          :: C f Word32
    , _brHash         :: C f PrivateHeaderHash
    , _brCreationTime :: C f UTCTime
    , _brPrevHash     :: C f PrivateHeaderHash
    , _brAtgDelta     :: C f ATGDelta
    , _brMerkleRoot   :: C f (MerkleSignature PrivateTx)
    , _brMerkleTree   :: C f (MerkleTree PrivateTx)
    } deriving (Generic)
makeLenses ''BlockRowT

data EducatorSchema f = EducatorSchema
    { _esCourses            :: f (TableEntity CourseRowT)
    , _esSubjects           :: f (TableEntity SubjectRowT)
    , _esStudents           :: f (TableEntity StudentRowT)
    , _esStudentCourses     :: f (TableEntity $ RelationT 'MxM (Id Student) (Id Course))
    , _esAssignments        :: f (TableEntity AssignmentRowT)
    , _esStudentAssignments :: f (TableEntity $ RelationT 'MxM (Id Student) (Id Assignment))
    , _esSubmissions        :: f (TableEntity SubmissionRowT)
    , _esTransactions       :: f (TableEntity TransactionRowT)
    , _esBlocks             :: f (TableEntity BlockRowT)
    , _esBlockTxs           :: f (TableEntity $ RelationT 'Mx1 (Id PrivateTx) Word32)
    } deriving (Generic)
makeLenses ''EducatorSchema

----------------------------------------------------------------------------
-- Connection with core types
----------------------------------------------------------------------------

class RowIso ty where
    type RowType ty = (rowTy :: (* -> *) -> *) | rowTy -> ty

    type RowExtras ty :: *
    type RowExtras ty = ()

    fromRowType :: RowType ty Identity -> ty
    toRowType :: RowExtras ty -> ty -> RowType ty Identity

fromRowTypesM :: (RowIso ty, Functor m) => m [RowType ty Identity] -> m [ty]
fromRowTypesM = fmap (map fromRowType)

instance RowIso Assignment where
    type RowType Assignment = AssignmentRowT
    fromRowType AssignmentRow{..} =
        Assignment
        { _aCourseId = _arCourse
        , _aContentsHash = _arContentsHash
        , _aType = _arType
        , _aDesc = _arDesc
        }
    toRowType () assignment@Assignment{..} =
        AssignmentRow
        { _arHash = hash assignment
        , _arCourse = _aCourseId
        , _arContentsHash = _aContentsHash
        , _arType = _aType
        , _arDesc = _aDesc
        }

instance RowIso PrivateTx where
    type RowType PrivateTx = TransactionRowT
    fromRowType TransactionRow{..} =
        error "PrivateTx requires submission itself, not its id"

    toRowType () PrivateTx{..} =
        error "Mememe"

----------------------------------------------------------------------------
-- Aliases
----------------------------------------------------------------------------

-- TODO remove?
type CourseRow = CourseRowT Identity
type RelationRow t a b = RelationT t a b Identity

----------------------------------------------------------------------------
-- 'Table' instances
----------------------------------------------------------------------------

instance (Typeable a, Typeable b) => Table (RelationT 'Mx1 a b) where
    data PrimaryKey (RelationT 'Mx1 a b) f = Mx1RelationRowId (Columnar f a)
        deriving (Generic)
    primaryKey (a :-: _) = Mx1RelationRowId a

instance (Typeable a, Typeable b) => Table (RelationT 'MxM a b) where
    data PrimaryKey (RelationT 'MxM a b) f = MxMRelationRowId (Columnar f a) (Columnar f b)
        deriving (Generic)
    primaryKey (a :-: b) = MxMRelationRowId a b

instance Table CourseRowT where
    data PrimaryKey CourseRowT f = CourseRowId (C f (Id Course))
        deriving (Generic)
    primaryKey = CourseRowId . _crId

instance Table SubjectRowT where
    data PrimaryKey SubjectRowT f = SubjectRowId (C f (Id Subject))
        deriving (Generic)
    primaryKey = SubjectRowId . _srId

instance Table StudentRowT where
    data PrimaryKey StudentRowT f = StudentRowId (C f (Id Student))
        deriving (Generic)
    primaryKey = StudentRowId . _srAddr

instance Table AssignmentRowT where
    data PrimaryKey AssignmentRowT f = AssignmentRowId (C f (Id Assignment))
        deriving (Generic)
    primaryKey = AssignmentRowId . _arHash

instance Table SubmissionRowT where
    data PrimaryKey SubmissionRowT f = SubmissionRowId (C f (Id Submission))
        deriving (Generic)
    primaryKey = SubmissionRowId . _srHash

instance Table TransactionRowT where
    data PrimaryKey TransactionRowT f = TransactionRowId (C f (Id PrivateTx))
        deriving (Generic)
    primaryKey = TransactionRowId . _trHash

instance Table BlockRowT where
    data PrimaryKey BlockRowT f = BlockRowId (C f Word32)
        deriving (Generic)
    primaryKey = BlockRowId . _brIdx

----------------------------------------------------------------------------
-- 'Beamable' instances
----------------------------------------------------------------------------

instance Beamable (RelationT t a b)
instance Beamable (PrimaryKey $ RelationT 'Mx1 a b)
instance Beamable (PrimaryKey $ RelationT 'MxM a b)

instance Beamable CourseRowT
instance Beamable (PrimaryKey CourseRowT)

instance Beamable SubjectRowT
instance Beamable (PrimaryKey SubjectRowT)

instance Beamable StudentRowT
instance Beamable (PrimaryKey StudentRowT)

instance Beamable AssignmentRowT
instance Beamable (PrimaryKey AssignmentRowT)

instance Beamable SubmissionRowT
instance Beamable (PrimaryKey SubmissionRowT)

instance Beamable TransactionRowT
instance Beamable (PrimaryKey TransactionRowT)

instance Beamable BlockRowT
instance Beamable (PrimaryKey BlockRowT)

----------------------------------------------------------------------------
-- Final
----------------------------------------------------------------------------

instance Database be EducatorSchema

-- TODO: do we care about table and field names in resulting schema?
educatorSchema :: DatabaseSettings be EducatorSchema
educatorSchema = defaultDbSettings

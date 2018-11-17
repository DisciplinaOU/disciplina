{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Dscp.DB.SQLite.Schema
    ( module Dscp.DB.SQLite.Schema
    ) where

import Prelude hiding (_1, _2)

import Control.Lens (Field1 (..), Field2 (..))
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
    _1 f (a :-: b) = (:-: b) <$> f a
instance (rel ~ RelationT t a b f, cb ~ Columnar f b) => Field2 rel rel cb cb where
    _2 f (a :-: b) = (a :-:) <$> f b


data CourseRowT f = CourseRow
    { crId   :: C f Course
    , crDesc :: C f Text
    } deriving (Generic)

data SubjectRowT f = SubjectRow
    { srId     :: C f Subject
    , srCourse :: C f Course
    , srDesc   :: C f Text
    } deriving (Generic)

data StudentRowT f = StudentRow
    { srAddr     :: C f Address
    } deriving (Generic)

data AssignmentRowT f = AssignmentRow
    { arHash         :: C f (Hash Assignment)
    , arCourse       :: C f (Id Course)
    , arContentsHash :: C f (Hash Raw)
    , arType         :: C f AssignmentType
    , arDesc         :: C f Text
    } deriving (Generic)

data SubmissionRowT f = SubmissionRow
    { srHash           :: C f (Hash Submission)
    , srStudent        :: C f Student
    , srAssignmentHash :: C f (Hash Assignment)
    , srContentsHash   :: C f (Hash Raw)
    , srSignature      :: C f SubmissionWitness
    , srCreationTime   :: C f UTCTime
    } deriving (Generic)

data TransactionRowT f = TransactionRow
    { trHash           :: C f (Hash PrivateTx)
    , trSubmissionHash :: C f (Hash Submission)
    , trGrade          :: C f Grade
    , trCreationTime   :: C f UTCTime
    , trIdx            :: C f TxBlockIdx
    } deriving (Generic)

-- We need `idx` field to be able to perform queries like "get N last blocks" efficiently.
data BlockRowT f = BlockRow
    { brIdx          :: C f Word32
    , brHash         :: C f PrivateHeaderHash
    , brCreationTime :: C f UTCTime
    , brPrevHash     :: C f PrivateHeaderHash
    , brAtgDelta     :: C f ATGDelta
    , brMerkleRoot   :: C f (MerkleSignature PrivateTx)
    , brMerkleTree   :: C f (MerkleTree PrivateTx)
    } deriving (Generic)

data EducatorSchema f = EducatorSchema
    { esCourses            :: f (TableEntity CourseRowT)
    , esSubjects           :: f (TableEntity SubjectRowT)
    , esStudents           :: f (TableEntity StudentRowT)
    , esStudentCourses     :: f (TableEntity $ RelationT 'MxM (Id Student) (Id Course))
    , esAssignments        :: f (TableEntity AssignmentRowT)
    , esStudentAssignments :: f (TableEntity $ RelationT 'MxM (Id Student) (Id Assignment))
    , esSubmissions        :: f (TableEntity SubmissionRowT)
    , esTransactions       :: f (TableEntity TransactionRowT)
    , esBlocks             :: f (TableEntity BlockRowT)
    , esBlockTxs           :: f (TableEntity $ RelationT 'Mx1 (Id PrivateTx) Word32)
    } deriving (Generic)

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
        { _aCourseId = arCourse
        , _aContentsHash = arContentsHash
        , _aType = arType
        , _aDesc = arDesc
        }
    toRowType () assignment@Assignment{..} =
        AssignmentRow
        { arHash = hash assignment
        , arCourse = _aCourseId
        , arContentsHash = _aContentsHash
        , arType = _aType
        , arDesc = _aDesc
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
    primaryKey = CourseRowId . crId

instance Table SubjectRowT where
    data PrimaryKey SubjectRowT f = SubjectRowId (C f (Id Subject))
        deriving (Generic)
    primaryKey = SubjectRowId . srId

instance Table StudentRowT where
    data PrimaryKey StudentRowT f = StudentRowId (C f (Id Student))
        deriving (Generic)
    primaryKey = StudentRowId . srAddr

instance Table AssignmentRowT where
    data PrimaryKey AssignmentRowT f = AssignmentRowId (C f (Id Assignment))
        deriving (Generic)
    primaryKey = AssignmentRowId . arHash

instance Table SubmissionRowT where
    data PrimaryKey SubmissionRowT f = SubmissionRowId (C f (Id Submission))
        deriving (Generic)
    primaryKey = SubmissionRowId . srHash

instance Table TransactionRowT where
    data PrimaryKey TransactionRowT f = TransactionRowId (C f (Id PrivateTx))
        deriving (Generic)
    primaryKey = TransactionRowId . trHash

instance Table BlockRowT where
    data PrimaryKey BlockRowT f = BlockRowId (C f Word32)
        deriving (Generic)
    primaryKey = BlockRowId . brIdx

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

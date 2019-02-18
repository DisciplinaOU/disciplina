{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Dscp.Educator.DB.Schema
    ( module Dscp.Educator.DB.Schema
    ) where

import Prelude hiding (_1, _2)

import Database.Beam.Backend (runNoReturn)
import Database.Beam.Postgres (PgJSONB)
import Database.Beam.Postgres.Syntax (PgCommandSyntax (..), PgCommandType (..), emit)
import Database.Beam.Schema.Tables (Beamable, C, Database, DatabaseSettings, Table (..),
                                    TableEntity, defaultDbSettings)
import Pdf.Scanner (PDFBody)

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQL.Functions
import Dscp.DB.SQL.Util
import Dscp.Educator.DB.BlockData
import Dscp.Educator.DB.FileQuoter
import Dscp.Educator.DB.Instances ()
import Dscp.Util

----------------------------------------------------------------------------
-- Tables
----------------------------------------------------------------------------

data CourseRowT f = CourseRow
    { crId   :: C f Course
    , crDesc :: C f ItemDesc
    } deriving (Generic)

data SubjectRowT f = SubjectRow
    { srId     :: C f Subject
    , srDesc   :: C f ItemDesc
    , srCourse :: PrimaryKey CourseRowT f
    } deriving (Generic)

data StudentRowT f = StudentRow
    { srAddr     :: C f Address
    } deriving (Generic)

data AssignmentRowT f = AssignmentRow
    { arHash         :: C f (Hash Assignment)
    , arContentsHash :: C f (Hash Raw)
    , arType         :: C f AssignmentType
    , arDesc         :: C f ItemDesc
    , arCourse       :: PrimaryKey CourseRowT f
    } deriving (Generic)

data SubmissionRowT f = SubmissionRow
    { srHash         :: C f (Hash Submission)
    , srContentsHash :: C f (Hash Raw)
    , srSignature    :: C f SubmissionWitness
    , srCreationTime :: C f Timestamp
    , srStudent      :: PrimaryKey StudentRowT f
    , srAssignment   :: PrimaryKey AssignmentRowT f
    } deriving (Generic)

data TransactionRowT f = TransactionRow
    { trHash         :: C f (Hash PrivateTx)
    , trGrade        :: C f Grade
    , trCreationTime :: C f Timestamp
    , trIdx          :: C f TxBlockIdx
    , trSubmission   :: PrimaryKey SubmissionRowT f
    } deriving (Generic)

-- We need `idx` field to be able to perform queries like "get N last blocks" efficiently.
data BlockRowT f = BlockRow
    { brIdx          :: C f BlockIdx
    , brHash         :: C f PrivateHeaderHash
    , brCreationTime :: C f Timestamp
    , brPrevHash     :: C f PrivateHeaderHash
    , brAtgDelta     :: C f ATGDelta
    , brMerkleRoot   :: C f (MerkleSignature PrivateTx)
    , brMerkleTree   :: C f (EmptyMerkleTree PrivateTx)
    } deriving (Generic)

data CertificateRowT f = CertificateRow
    { crHash :: C f (Hash CertificateMeta)
    , crMeta :: C f (PgJSONB CertificateMeta)
    , crPdf  :: C f PDFBody
    } deriving (Generic)

data EducatorSchema f = EducatorSchema
    { esCourses             :: f (TableEntity CourseRowT)
    , esSubjects            :: f (TableEntity SubjectRowT)
    , esStudents            :: f (TableEntity StudentRowT)
    , esStudentCourses      :: f (TableEntity $ RelationT 'MxM StudentRowT CourseRowT)
    , esAssignments         :: f (TableEntity AssignmentRowT)
    , esStudentAssignments  :: f (TableEntity $ RelationT 'MxM StudentRowT AssignmentRowT)
    , esSubmissions         :: f (TableEntity SubmissionRowT)
    , esTransactions        :: f (TableEntity TransactionRowT)
    , esBlocks              :: f (TableEntity BlockRowT)
    , esBlockTxs            :: f (TableEntity $ RelationT 'Mx1 TransactionRowT BlockRowT)

    , esCertificates        :: f (TableEntity CertificateRowT)
    , esCertificatesVersion :: f (TableEntity $ SingletonT Word32)
    } deriving (Generic)

----------------------------------------------------------------------------
-- Aliases
----------------------------------------------------------------------------

type CourseRow = CourseRowT Identity
type SubjectRow = SubjectRowT Identity
type StudentRow = StudentRowT Identity
type AssignmentRow = AssignmentRowT Identity
type SubmissionRow = SubmissionRowT Identity
type TransactionRow = TransactionRowT Identity
type BlockRow = BlockRowT Identity
type CertificateRow = CertificateRowT Identity

----------------------------------------------------------------------------
-- Connection with core types
----------------------------------------------------------------------------

-- TODO [DSCP-383] Fetch less info
assignmentFromRow :: AssignmentRow -> Assignment
assignmentFromRow AssignmentRow{..} =
    Assignment
    { _aCourseId = unpackPk arCourse
    , _aContentsHash = arContentsHash
    , _aType = arType
    , _aDesc = arDesc
    }

submissionFromRow :: SubmissionRow -> SignedSubmission
submissionFromRow SubmissionRow{..} =
    SignedSubmission
    { _ssSubmission = Submission
        { _sStudentId = unpackPk srStudent
        , _sContentsHash = srContentsHash
        , _sAssignmentHash = unpackPk srAssignment
        }
    , _ssWitness = srSignature
    }

privateTxFromRow :: (TransactionRow, SubmissionRow) -> PrivateTx
privateTxFromRow (TransactionRow{..}, sub) =
    PrivateTx
    { _ptSignedSubmission = submissionFromRow sub
    , _ptGrade = trGrade
    , _ptTime = trCreationTime
    }

pbHeaderFromRow :: BlockRow -> PrivateBlockHeader
pbHeaderFromRow BlockRow{..} =
    PrivateBlockHeader
    { _pbhPrevBlock = brPrevHash
    , _pbhBodyProof = brMerkleRoot
    , _pbhAtgDelta = brAtgDelta
    }

----------------------------------------------------------------------------
-- 'Table' instances
----------------------------------------------------------------------------

instance Table CourseRowT where
    newtype PrimaryKey CourseRowT f = CourseRowId (C f (Id Course))
        deriving (Generic)
    primaryKey = CourseRowId . crId

instance Table SubjectRowT where
    newtype PrimaryKey SubjectRowT f = SubjectRowId (C f (Id Subject))
        deriving (Generic)
    primaryKey = SubjectRowId . srId

instance Table StudentRowT where
    newtype PrimaryKey StudentRowT f = StudentRowId (C f (Id Student))
        deriving (Generic)
    primaryKey = StudentRowId . srAddr

instance Table AssignmentRowT where
    newtype PrimaryKey AssignmentRowT f = AssignmentRowId (C f (Id Assignment))
        deriving (Generic)
    primaryKey = AssignmentRowId . arHash

instance Table SubmissionRowT where
    newtype PrimaryKey SubmissionRowT f = SubmissionRowId (C f (Id Submission))
        deriving (Generic)
    primaryKey = SubmissionRowId . srHash

instance Table TransactionRowT where
    newtype PrimaryKey TransactionRowT f = TransactionRowId (C f (Id PrivateTx))
        deriving (Generic)
    primaryKey = TransactionRowId . trHash

instance Table BlockRowT where
    newtype PrimaryKey BlockRowT f = BlockRowId (C f BlockIdx)
        deriving (Generic)
    primaryKey = BlockRowId . brIdx

instance Table CertificateRowT where
    newtype PrimaryKey CertificateRowT f = HashTableId (C f (Hash CertificateMeta))
        deriving (Generic)
    primaryKey = HashTableId . crHash

----------------------------------------------------------------------------
-- 'Beamable' instances
----------------------------------------------------------------------------

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

instance Beamable CertificateRowT
instance Beamable (PrimaryKey CertificateRowT)

----------------------------------------------------------------------------
-- Final
----------------------------------------------------------------------------

instance Database be EducatorSchema

-- | Educator schema description.
-- The schema is assumed to have names of form "student_courses" (for tables),
-- "creation_time" (for fields) or "course__id" (for foreign keys).
educatorSchema :: DatabaseSettings be EducatorSchema
educatorSchema = defaultDbSettings

-- | Schema definition in raw SQL.
schemaDefinition :: IsString s => s
schemaDefinition = [qFile|./database/schema.sql|]

-- | Create tables if absent.
ensureSchemaIsSetUp :: MonadIO m => DBT 'WithinTx m ()
ensureSchemaIsSetUp =
    liftPg . runNoReturn $ PgCommandSyntax PgCommandTypeDataUpdate $ emit schemaDefinition

{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Dscp.Educator.DB.Schema
    ( module Dscp.Educator.DB.Schema
    ) where

import Universum hiding (_1, _2)

import qualified Data.Aeson as A
import Database.Beam.Backend (runNoReturn)
import Database.Beam.Postgres.Syntax (PgCommandSyntax (..), PgCommandType (..), emit)
import Database.Beam.Schema.Tables (Beamable, C, Database, Nullable, Table (..), TableEntity,
                                    defaultDbSettings)
import Pdf.Scanner (PDFBody)
import System.FilePath.Posix ((</>))

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQL.Functions
import Dscp.DB.SQL.Util
import Dscp.Educator.DB.BlockData
import Dscp.Educator.DB.Instances ()
import Dscp.Util
import Dscp.Util.FileEmbed

----------------------------------------------------------------------------
-- Tables
----------------------------------------------------------------------------

data TransactionRowT f = TransactionRow
    { trHash         :: C f (Hash PrivateTx)
    , trCreationTime :: C f Timestamp
    , trIdx          :: C f TxBlockIdx
    , trEntity       :: C f Entity
    , trData         :: C f (PgJSONB A.Value)
    } deriving (Generic)

-- We need `idx` field to be able to perform queries like "get N last blocks" efficiently.
data BlockRowT f = BlockRow
    { brIdx          :: C f BlockIdx
    , brHash         :: C f PrivateHeaderHash
    , brCreationTime :: C f Timestamp
    , brPrevHash     :: C f PrivateHeaderHash
    , brPubTxId      :: C (Nullable f) PubTxId
    , brMerkleRoot   :: C f (MerkleSignature PrivateTx)
    , brMerkleTree   :: C f (EmptyMerkleTree PrivateTx)
    } deriving (Generic)

data CertificateRowT f = CertificateRow
    { crHash   :: C f (Hash CertificateMeta)
    , crMeta   :: C f (PgJSONB CertificateMeta)
    , crPdf    :: C f PDFBody
    } deriving (Generic)

data EducatorSchema f = EducatorSchema
    { esTransactions        :: f (TableEntity TransactionRowT)
    , esBlocks              :: f (TableEntity BlockRowT)
    , esBlockTxs            :: f (TableEntity $ RelationT 'Mx1 TransactionRowT BlockRowT)

    , esCertificates        :: f (TableEntity CertificateRowT)
    , esCertificateBlocks   :: f (TableEntity $ RelationT 'MxM BlockRowT CertificateRowT)
    , esCertificatesVersion :: f (TableEntity $ SingletonT Word32)
    } deriving (Generic)

----------------------------------------------------------------------------
-- Aliases
----------------------------------------------------------------------------

type TransactionRow = TransactionRowT Identity
type BlockRow = BlockRowT Identity
type CertificateRow = CertificateRowT Identity

----------------------------------------------------------------------------
-- Connection with core types
----------------------------------------------------------------------------

privateTxFromRow :: TransactionRow -> PrivateTx
privateTxFromRow TransactionRow{..} =
    PrivateTx
    { _ptEntity = trEntity
    , _ptData = case trData of PgJSONB d -> d
    , _ptTime = trCreationTime
    }

pbHeaderFromRow :: BlockRow -> PrivateBlockHeader
pbHeaderFromRow BlockRow{..} =
    PrivateBlockHeader
    { _pbhPrevBlock = brPrevHash
    , _pbhBodyProof = brMerkleRoot
    }

----------------------------------------------------------------------------
-- 'Table' instances
----------------------------------------------------------------------------

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
schemaDefinition =
    $(embedSubprojectStringFile "educator" ("database" </> "schema.sql"))

-- | Create tables if absent.
ensureSchemaIsSetUp :: MonadIO m => DBT 'WithinTx m ()
ensureSchemaIsSetUp =
    liftPg . runNoReturn $ PgCommandSyntax PgCommandTypeDataUpdate $ emit schemaDefinition

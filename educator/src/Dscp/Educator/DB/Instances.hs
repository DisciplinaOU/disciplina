{-# LANGUAGE CPP         #-}
{-# LANGUAGE QuasiQuotes #-}

module Dscp.Educator.DB.Instances () where

import Codec.Serialise as Codec (deserialise)
import qualified Data.ByteArray as BA
import Database.Beam.Backend (BackendFromField, BeamBackend, FromBackendRow (..))
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax (..), IsSql92ExpressionSyntax,
                                        Sql92ExpressionValueSyntax)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Database.Beam.Query (HasSqlEqualityCheck (..))
import Database.PostgreSQL.Simple.FromField (FromField (..))

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.DB.BlockData
import Dscp.Util
import Dscp.Util.Serialise

----------------------------------------------------------------------------
-- SQL serialisation instances
----------------------------------------------------------------------------

{- Instance templates

CPP does not allow multi-line output, so writing one macros per instance.
TH would play better, but supposedly would also work slightly slower.
-}

#define IsSqliteValue HasSqlValueSyntax PgValueSyntax

#define CodecInstanceEnc(TYPE) \
instance IsSqliteValue (TYPE) where \
    sqlValueSyntax = sqlValueSyntax . serialise'

#define CodecInstanceDec(TYPE) \
instance FromField (TYPE) where \
    fromField field ty = Codec.deserialise <$> fromField field ty

#define ByteArrayInstanceEnc(TYPE) \
instance IsSqliteValue (TYPE) where \
    sqlValueSyntax = sqlValueSyntax . BA.convert @_ @ByteString

#define ByteArrayInstanceDec(TYPE) \
instance FromField (TYPE) where \
    fromField field ty = \
        leftToPanic . fromByteArray @(TYPE) @ByteString <$> fromField field ty

#define EnumInstanceEnc(TYPE) \
instance IsSqliteValue (TYPE) where \
    sqlValueSyntax = sqlValueSyntax . fromEnum

#define EnumInstanceDec(TYPE) \
instance FromField (TYPE) where \
    fromField field ty = toEnum <$> fromField field ty

{- Instances via Enum -}

EnumInstanceEnc(AssignmentType)
EnumInstanceDec(AssignmentType)

EnumInstanceEnc(DocumentType)
EnumInstanceDec(DocumentType)

{- Instances via FromByteArray -}

ByteArrayInstanceEnc(Hash a)
ByteArrayInstanceDec(Hash a)

ByteArrayInstanceEnc(Signature a)
ByteArrayInstanceDec(Signature a)

{- Instances via Serialise -}

CodecInstanceEnc(Address)
CodecInstanceDec(Address)

CodecInstanceEnc(ATGDelta)
CodecInstanceDec(ATGDelta)

CodecInstanceEnc(SubmissionWitness)
CodecInstanceDec(SubmissionWitness)

CodecInstanceEnc(MerkleSignature a)
CodecInstanceDec(MerkleSignature a)

CodecInstanceEnc(EmptyMerkleTree a)
CodecInstanceDec(EmptyMerkleTree a)

{- Newtype-derived instances -}

deriving instance FromField Subject
deriving instance IsSqliteValue Subject

deriving instance FromField Course
deriving instance IsSqliteValue Course

deriving instance FromField Grade
deriving instance IsSqliteValue Grade

deriving instance FromField BlockIdx
deriving instance IsSqliteValue BlockIdx

{- Custom instances -}

instance FromField TxBlockIdx where
    fromField field ty = leftToPanic . txBlockIdxFromInt <$> fromField field ty

instance HasSqlValueSyntax PgValueSyntax TxBlockIdx where
    sqlValueSyntax = sqlValueSyntax . txBlockIdxToInt

{- Basic instances -}

instance FromField Word32 where
    fromField field ty = fromIntegral @Int32 <$> fromField field ty

instance FromField Word8 where
    fromField field ty = fromIntegralChecked @_ @Int16 <$> fromField field ty

----------------------------------------------------------------------------
-- 'FromBackendRow' instances
----------------------------------------------------------------------------

#define GenFromBackendRow(TYPE) \
instance (BeamBackend be, BackendFromField be (TYPE)) => FromBackendRow be (TYPE)

-- For SQLite they all refer to 'FromField' instances
GenFromBackendRow(Hash a)
GenFromBackendRow(Address)
GenFromBackendRow(Course)
GenFromBackendRow(AssignmentType)
GenFromBackendRow(Subject)
GenFromBackendRow(Grade)
GenFromBackendRow(BlockIdx)
GenFromBackendRow(TxBlockIdx)
GenFromBackendRow(SubmissionWitness)
GenFromBackendRow(MerkleSignature a)
GenFromBackendRow(EmptyMerkleTree a)
GenFromBackendRow(ATGDelta)

----------------------------------------------------------------------------
-- Other instances
----------------------------------------------------------------------------

#define GenHasSqlEqualityCheck(TYPE) \
instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax syntax) (TYPE), \
          IsSql92ExpressionSyntax syntax) => \
         HasSqlEqualityCheck syntax (TYPE)

GenHasSqlEqualityCheck(Address)
GenHasSqlEqualityCheck(Course)
GenHasSqlEqualityCheck(Subject)
GenHasSqlEqualityCheck(Grade)
GenHasSqlEqualityCheck(TxBlockIdx)
GenHasSqlEqualityCheck(BlockIdx)
GenHasSqlEqualityCheck(Hash a)
GenHasSqlEqualityCheck(AssignmentType)

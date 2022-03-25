{-# LANGUAGE CPP         #-}
{-# LANGUAGE QuasiQuotes #-}

module Dscp.Educator.DB.Instances () where

import Universum

import Codec.Serialise as Codec (deserialise)
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray as BA
import Data.ByteArray.HexString (HexString)
import Data.Time.Clock (UTCTime)
import Database.Beam.Backend (BackendFromField, BeamBackend, BeamSqlBackend, FromBackendRow (..))
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax (..))
import Database.Beam.Migrate (HasDefaultSqlDataType (..))
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Database.Beam.Query (HasSqlEqualityCheck (..))
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Pdf.Scanner (PDFBody (..))

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

#define IsPgValue HasSqlValueSyntax PgValueSyntax

#define CodecInstanceEnc(TYPE) \
instance IsPgValue (TYPE) where \
    sqlValueSyntax = sqlValueSyntax . serialise'

#define CodecInstanceDec(TYPE) \
instance FromField (TYPE) where \
    fromField field ty = Codec.deserialise <$> fromField field ty

#define ByteArrayInstanceEnc(TYPE) \
instance IsPgValue (TYPE) where \
    sqlValueSyntax = sqlValueSyntax . BA.convert @_ @ByteString

#define ByteArrayInstanceDec(TYPE) \
instance FromField (TYPE) where \
    fromField field ty = \
        leftToPanic . fromByteArray @(TYPE) @ByteString <$> fromField field ty

#define JsonInstanceEnc(TYPE) \
instance IsPgValue (TYPE) where \
    sqlValueSyntax = sqlValueSyntax . Aeson.encode

#define JsonInstanceDec(TYPE) \
instance FromField (TYPE) where \
    fromField field ty = leftToPanic . Aeson.eitherDecode <$> fromField field ty

#define EnumInstanceEnc(TYPE) \
instance IsPgValue (TYPE) where \
    sqlValueSyntax = sqlValueSyntax . fromIntegral @Int @Int64 . fromEnum

#define EnumInstanceDec(TYPE) \
instance FromField (TYPE) where \
    fromField field ty = toEnum <$> fromField field ty

{- Instances via Enum -}

EnumInstanceEnc(AssignmentType)
EnumInstanceDec(AssignmentType)

EnumInstanceEnc(DocumentType a)
EnumInstanceDec(DocumentType a)

{- Instances via FromByteArray -}

instance FromByteArray HexString

ByteArrayInstanceEnc(HexString)
ByteArrayInstanceDec(HexString)

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

{- Instances via JSON -}

JsonInstanceEnc(CertificateMeta)
JsonInstanceDec(CertificateMeta)

{- Newtype-derived instances -}

deriving instance IsPgValue ItemDesc

deriving instance IsPgValue Timestamp

deriving instance FromField Subject
deriving instance IsPgValue Subject

deriving instance FromField Course
deriving instance IsPgValue Course

deriving instance FromField Grade
deriving instance IsPgValue Grade

deriving instance FromField BlockIdx
deriving instance IsPgValue BlockIdx

deriving instance FromField PDFBody
deriving instance IsPgValue PDFBody

{- Custom instances -}

instance FromField TxBlockIdx where
    fromField field ty = leftToPanic . txBlockIdxFromInt <$> fromField field ty

instance HasSqlValueSyntax PgValueSyntax TxBlockIdx where
    sqlValueSyntax = sqlValueSyntax . fromIntegral @Int @Int64 . txBlockIdxToInt

{- Basic instances -}

instance FromField Word32 where
    fromField field ty = fromIntegral @Int32 <$> fromField field ty

instance FromField Word8 where
    fromField field ty = fromIntegralChecked @_ @Int16 <$> fromField field ty

instance FromField ItemDesc where
    fromField field ty = toItemDescUnsafe <$> fromField @Text field ty

instance FromField Timestamp where
    fromField field ty = toTimestampUnsafe <$> fromField @UTCTime field ty

----------------------------------------------------------------------------
-- 'FromBackendRow' instances
----------------------------------------------------------------------------

#define GenFromBackendRow(TYPE) \
instance (BeamBackend be, BackendFromField be (TYPE)) => FromBackendRow be (TYPE)

#define GenFromBackendRow2arity(TYPE) \
instance (Typeable a, BeamBackend be, BackendFromField be (TYPE a)) => FromBackendRow be (TYPE a)


-- For Postgres they all refer to 'FromField' instances
GenFromBackendRow(ItemDesc)
GenFromBackendRow(Timestamp)
GenFromBackendRow(HexString)
GenFromBackendRow2arity(Hash)
GenFromBackendRow(Address)
GenFromBackendRow(Course)
GenFromBackendRow(AssignmentType)
GenFromBackendRow(Subject)
GenFromBackendRow(Grade)
GenFromBackendRow(BlockIdx)
GenFromBackendRow(TxBlockIdx)
GenFromBackendRow(SubmissionWitness)
GenFromBackendRow2arity(MerkleSignature)
GenFromBackendRow2arity(EmptyMerkleTree)
GenFromBackendRow(ATGDelta)
GenFromBackendRow(CertificateMeta)
GenFromBackendRow(PDFBody)

----------------------------------------------------------------------------
-- Other instances
----------------------------------------------------------------------------

#define GenHasSqlEqualityCheck(TYPE) \
instance BeamSqlBackend be => HasSqlEqualityCheck be (TYPE)

GenHasSqlEqualityCheck(Address)
GenHasSqlEqualityCheck(Course)
GenHasSqlEqualityCheck(Subject)
GenHasSqlEqualityCheck(Grade)
GenHasSqlEqualityCheck(TxBlockIdx)
GenHasSqlEqualityCheck(BlockIdx)
GenHasSqlEqualityCheck(HexString)
GenHasSqlEqualityCheck(Hash a)
GenHasSqlEqualityCheck(AssignmentType)


instance HasDefaultSqlDataType Postgres ItemDesc where
    defaultSqlDataType _ = defaultSqlDataType (Proxy @Text)

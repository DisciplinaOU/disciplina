{-# LANGUAGE CPP         #-}
{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.Instances () where

import Codec.Serialise as Codec (deserialise)
import qualified Data.ByteArray as BA
import Database.Beam.Backend (FromBackendRow (..))
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax (..))
import Database.Beam.Query (HasSqlEqualityCheck (..))
import Database.Beam.Sqlite (Sqlite)
import Database.Beam.Sqlite.Syntax (SqliteExpressionSyntax, SqliteValueSyntax)
import Database.SQLite.Simple.FromField (FromField (..))

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQLite.BlockData
import Dscp.Util
import Dscp.Util.Serialise

----------------------------------------------------------------------------
-- SQL serialisation instances
----------------------------------------------------------------------------

{- Instance templates

CPP does not allow multi-line output, so writing one macros per instance.
TH would play better, but supposedly would also work slightly slower.
-}

#define IsSqliteValue HasSqlValueSyntax SqliteValueSyntax

#define CodecInstanceEnc(TYPE) \
instance IsSqliteValue (TYPE) where \
    sqlValueSyntax = sqlValueSyntax . serialise'

#define CodecInstanceDec(TYPE) \
instance FromField (TYPE) where \
    fromField f = Codec.deserialise <$> fromField f

#define ByteArrayInstanceEnc(TYPE) \
instance IsSqliteValue (TYPE) where \
    sqlValueSyntax = sqlValueSyntax . BA.convert @_ @ByteString

#define ByteArrayInstanceDec(TYPE) \
instance FromField (TYPE) where \
    fromField f = leftToPanic . fromByteArray @(TYPE) @ByteString <$> fromField f

#define EnumInstanceEnc(TYPE) \
instance IsSqliteValue (TYPE) where \
    sqlValueSyntax = sqlValueSyntax . fromEnum

#define EnumInstanceDec(TYPE) \
instance FromField (TYPE) where \
    fromField f = toEnum <$> fromField f

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
    fromField f = leftToPanic . txBlockIdxFromInt <$> fromField f

instance HasSqlValueSyntax SqliteValueSyntax TxBlockIdx where
    sqlValueSyntax = sqlValueSyntax . txBlockIdxToInt

----------------------------------------------------------------------------
-- 'FromBackendRow' instances
----------------------------------------------------------------------------

-- For SQLite they all refer to 'FromField' instances
instance FromBackendRow Sqlite (Hash a)
instance FromBackendRow Sqlite Address
instance FromBackendRow Sqlite Course
instance FromBackendRow Sqlite AssignmentType
instance FromBackendRow Sqlite Subject
instance FromBackendRow Sqlite Grade
instance FromBackendRow Sqlite BlockIdx
instance FromBackendRow Sqlite TxBlockIdx where
instance FromBackendRow Sqlite SubmissionWitness where
instance FromBackendRow Sqlite (MerkleSignature a) where
instance FromBackendRow Sqlite (EmptyMerkleTree a) where
instance FromBackendRow Sqlite ATGDelta where

----------------------------------------------------------------------------
-- Other instances
----------------------------------------------------------------------------

instance HasSqlEqualityCheck SqliteExpressionSyntax Address
instance HasSqlEqualityCheck SqliteExpressionSyntax Course
instance HasSqlEqualityCheck SqliteExpressionSyntax TxBlockIdx
instance HasSqlEqualityCheck SqliteExpressionSyntax BlockIdx
instance HasSqlEqualityCheck SqliteExpressionSyntax (Hash a)

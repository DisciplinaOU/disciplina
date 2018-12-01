{-# LANGUAGE CPP         #-}
{-# LANGUAGE GADTs       #-}
{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.Instances
    ( BackendHasInstances
    , MonadQuery
    , MonadQueryEq
    , MonadQueryFull
    ) where

import Codec.Serialise as Codec (deserialise)
import qualified Data.ByteArray as BA
import Data.Time.Clock (UTCTime)
import Database.Beam (MonadBeam)
import Database.Beam.Backend (BackendFromField, BeamBackend, FromBackendRow (..))
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax (..), IsSql92ExpressionSyntax,
                                        Sql92ExpressionValueSyntax)
import qualified Database.Beam.Backend.SQL.SQL92 as Beam
import Database.Beam.Query (HasSqlEqualityCheck (..))
import qualified Database.Beam.Query as Beam
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax)
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

GenHasSqlEqualityCheck(Hash a)
GenHasSqlEqualityCheck(Address)
GenHasSqlEqualityCheck(Course)
GenHasSqlEqualityCheck(AssignmentType)
GenHasSqlEqualityCheck(Subject)
GenHasSqlEqualityCheck(Grade)
GenHasSqlEqualityCheck(TxBlockIdx)
GenHasSqlEqualityCheck(BlockIdx)
GenHasSqlEqualityCheck(SubmissionWitness)
GenHasSqlEqualityCheck(MerkleSignature a)
GenHasSqlEqualityCheck(EmptyMerkleTree a)
GenHasSqlEqualityCheck(ATGDelta)

----------------------------------------------------------------------------
-- Backend instances
----------------------------------------------------------------------------

type TypeHasBackendInstances cmd be a =
    ( FromBackendRow be a
    , BackendFromField be a
    , Beam.HasSqlValueSyntax
        (Beam.Sql92ExpressionValueSyntax
            (Beam.Sql92SelectTableExpressionSyntax
              (Beam.Sql92SelectSelectTableSyntax (Beam.Sql92SelectSyntax cmd))))
      a
    , Beam.HasSqlValueSyntax
        (Beam.Sql92ExpressionValueSyntax
            (Beam.Sql92InsertValuesExpressionSyntax
              (Beam.Sql92InsertValuesSyntax (Beam.Sql92InsertSyntax cmd))))
      a
    , Beam.HasSqlEqualityCheck
      (Beam.Sql92InsertValuesExpressionSyntax
          (Beam.Sql92InsertValuesSyntax
            (Beam.Sql92InsertSyntax cmd)))
      a
    )

type BackendHasInstances cmd be =
    ( Beam.HasQBuilder (Beam.Sql92SelectSyntax cmd)
    , Beam.IsSql92Syntax cmd
    , TypeHasBackendInstances cmd be Int
    , TypeHasBackendInstances cmd be Text
    , TypeHasBackendInstances cmd be Bool
    , TypeHasBackendInstances cmd be Address
    , TypeHasBackendInstances cmd be Course
    , TypeHasBackendInstances cmd be Subject
    , TypeHasBackendInstances cmd be Grade
    , TypeHasBackendInstances cmd be TxBlockIdx
    , TypeHasBackendInstances cmd be BlockIdx
    , TypeHasBackendInstances cmd be AssignmentType
    , TypeHasBackendInstances cmd be SubmissionWitness
    , TypeHasBackendInstances cmd be UTCTime
    , TypeHasBackendInstances cmd be ATGDelta
    , TypeHasBackendInstances cmd be (Hash LByteString)
    , TypeHasBackendInstances cmd be (Hash Assignment)
    , TypeHasBackendInstances cmd be (Hash Submission)
    , TypeHasBackendInstances cmd be (Hash PrivateTx)
    , TypeHasBackendInstances cmd be (Hash PrivateBlockHeader)
    , TypeHasBackendInstances cmd be (MerkleSignature PrivateTx)
    , TypeHasBackendInstances cmd be (EmptyMerkleTree PrivateTx)
    )

-- | Basic constraints among those which should suffice for any db endpoint.
type MonadQuery cmd be hdl m =
    ( Monad m
    , MonadCatch m
    , BackendHasInstances cmd be
    , MonadBeam cmd be hdl m
    )

-- | Equality constraints among those which should suffice for any db endpoint.
-- See db endpoints running functions for the motivation of such distinction.
type MonadQueryEq cmd be hdl (m :: * -> *) =
    ( Beam.Sql92ExpressionSelectSyntax
        (Beam.Sql92SelectTableExpressionSyntax
           (Beam.Sql92SelectSelectTableSyntax
              (Beam.Sql92SelectSyntax cmd))) ~ Beam.Sql92SelectSyntax cmd
    )

-- | All constraints which any endpoint should be satisfied with.
type MonadQueryFull cmd be hdl m =
    ( MonadQuery cmd be hdl m
    , MonadQueryEq cmd be hdl m
    )

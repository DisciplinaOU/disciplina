{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.Instances () where

import Codec.Serialise as Codec (deserialise, serialise)

import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Database.SQLite.Simple.ToField (ToField (..))
import Database.SQLite.Simple.ToRow (ToRow (..))


import Dscp.Core.Serialise ()
import Dscp.Core.Types (Address (..), Assignment (..), AssignmentType, CourseId (..), Grade (..),
                        SignedSubmission (..), SubjectId (..), Submission (..), SubmissionType,
                        SubmissionWitness (..))
import Dscp.Crypto (Hash, PublicKey, Signature, hash)
import Dscp.Educator.Txs (PrivateTx (..))

instance FromField (Hash a)       where fromField f = Codec.deserialise <$> fromField f
instance FromField (Signature a)  where fromField f = Codec.deserialise <$> fromField f

-- TODO(kir): use #define to generate macros
instance FromField Address           where fromField f = Codec.deserialise <$> fromField f
instance FromField PublicKey         where fromField f = Codec.deserialise <$> fromField f
instance FromField SubjectId         where fromField f = SubjectId         <$> fromField f
instance FromField CourseId          where fromField f = CourseId          <$> fromField f
instance FromField Grade             where fromField f = Codec.deserialise <$> fromField f
instance FromField AssignmentType    where fromField f = Codec.deserialise <$> fromField f
instance FromField SubmissionType    where fromField f = Codec.deserialise <$> fromField f
instance FromField SubmissionWitness where fromField f = Codec.deserialise <$> fromField f

instance ToField   (Hash a)          where toField = toField . Codec.serialise
instance ToField   (Signature a)     where toField = toField . Codec.serialise

instance ToField   Address           where toField = toField . Codec.serialise
instance ToField   CourseId          where toField = toField . getCourseId
instance ToField   AssignmentType    where toField = toField . Codec.serialise
instance ToField   SubmissionType    where toField = toField . Codec.serialise
instance ToField   Grade             where toField = toField . Codec.serialise
instance ToField   SubmissionWitness where toField = toField . Codec.serialise

instance FromRow   CourseId       where fromRow = field
instance FromRow   Grade          where fromRow = field

instance FromRow Assignment       where fromRow = Assignment       <$> field   <*> field <*> field
instance FromRow Submission       where fromRow = Submission       <$> field   <*> field <*> fromRow
instance FromRow SignedSubmission where fromRow = SignedSubmission <$> fromRow <*> field
instance FromRow PrivateTx        where fromRow = PrivateTx        <$> fromRow <*> field <*> field

instance ToRow Assignment where
    toRow task@ (Assignment course ty text) =
        [toField (hash task), toField course, toField ty, toField text]


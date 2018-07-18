{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.Instances () where

import Codec.Serialise as Codec (deserialise, serialise)

import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Database.SQLite.Simple.ToField (ToField (..))
import Database.SQLite.Simple.ToRow (ToRow (..))


import Dscp.Core.Serialise ()
import Dscp.Core.Types (Address (..), Assignment (..), AssignmentType, Course (..), Grade (..),
                        SignedSubmission (..), Subject (..), Submission (..),
                        SubmissionWitness (..))
import Dscp.Crypto (Hash, PublicKey, Signature, hash)
import Dscp.Educator.Txs (PrivateTx (..))

instance FromField (Hash a)       where fromField f = Codec.deserialise <$> fromField f
instance FromField (Signature a)  where fromField f = Codec.deserialise <$> fromField f

-- TODO(kir): use #define to generate macros
instance FromField Address           where fromField f = Codec.deserialise <$> fromField f
instance FromField PublicKey         where fromField f = Codec.deserialise <$> fromField f
instance FromField Subject           where fromField f = Subject           <$> fromField f
instance FromField Course            where fromField f = Course            <$> fromField f
instance FromField Grade             where fromField f = UnsafeGrade       <$> fromField f
instance FromField AssignmentType    where fromField f = Codec.deserialise <$> fromField f
instance FromField SubmissionWitness where fromField f = Codec.deserialise <$> fromField f

instance ToField   (Hash a)          where toField = toField . Codec.serialise
instance ToField   (Signature a)     where toField = toField . Codec.serialise

instance ToField   Address           where toField = toField . Codec.serialise
instance ToField   Course            where toField = toField . getCourseId
instance ToField   Subject           where toField = toField . getSubjectId
instance ToField   AssignmentType    where toField = toField . Codec.serialise
instance ToField   Grade             where toField = toField . getGrade
instance ToField   SubmissionWitness where toField = toField . Codec.serialise

instance FromRow   Course            where fromRow = field
instance FromRow   Grade             where fromRow = field

instance FromRow   Assignment        where fromRow = Assignment       <$> field   <*> field <*> field <*> field
instance FromRow   Submission        where fromRow = Submission       <$> field   <*> field <*> fromRow
instance FromRow   SignedSubmission  where fromRow = SignedSubmission <$> fromRow <*> field
instance FromRow   PrivateTx         where fromRow = PrivateTx        <$> fromRow <*> field <*> field

instance ToRow Assignment where
    toRow task@ (Assignment course contentsHash ty text) =
        [toField (hash task), toField course, toField contentsHash, toField ty, toField text]


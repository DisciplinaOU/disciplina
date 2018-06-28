
module Dscp.DB.DSL.Interpret.Sqlite3.Instances where

import Universum

import Codec.Serialise as Codec (deserialise, serialise)

import Data.ByteString.Lazy as Lazy (ByteString)

import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))

import Dscp.Core.Types (Address (..), Assignment (..), AssignmentType (..), CourseId (..),
                        Grade (..), SignedSubmission (..), StudentId, Submission (..),
                        SubmissionSig, SubmissionType (..), SubmissionWitness (..))
import Dscp.Core.Serialise ()
import Dscp.Educator.Txs (PrivateTx (..), PrivateTxId)

-- TODO(kir): split into separate .Instances module.
instance FromField Address where
    fromField f = Codec.deserialise <$> fromField f

instance FromField CourseId where
    fromField f = Codec.deserialise <$> fromField f

instance FromField Grade where
    fromField f = Codec.deserialise <$> fromField f

instance FromField SubmissionSig where
    fromField f = Codec.deserialise <$> fromField f

instance ToField PrivateTxId where
    toField = toField . serialise


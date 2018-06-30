
module Dscp.DB.SQLite.Instances where

import Codec.Serialise as Codec (deserialise, serialise)

import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))

import Dscp.Core.Types (Address (..), CourseId (..), Grade (..), SubmissionSig)
import Dscp.Core.Serialise ()
import Dscp.Educator.Txs (PrivateTxId)

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


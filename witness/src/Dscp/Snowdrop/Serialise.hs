--  I'm not sure how to structure serialisation instances for now.

module Dscp.Snowdrop.Serialise () where

import Codec.Serialise (Serialise (..))
import qualified Snowdrop.Model.Block as SD
import qualified Snowdrop.Model.State.Core as SD
import qualified Snowdrop.Util as SD

import Dscp.Core ()
import Dscp.Snowdrop.AccountValidation as A
import Dscp.Snowdrop.Configuration


deriving instance Generic (SD.ChangeSet id v)
deriving instance Generic (SD.WithSignature pk sig a)
deriving instance Generic (SD.BlockRef a)

instance Serialise v => Serialise (SD.ValueOp v)
instance (Serialise pk, Serialise sig, Serialise a) => Serialise (SD.WithSignature pk sig a)
instance (Ord id, Serialise id, Serialise v) => Serialise (SD.ChangeSet id v)
instance Serialise a => Serialise (SD.TipValue a)
instance Serialise a => Serialise (SD.BlockRef a)
instance Serialise SD.StateTxType
instance (Serialise id, Ord id, Serialise v, Serialise proof) => Serialise (SD.StateTx id proof v)
instance Serialise A.Account
instance Serialise A.Author
instance Serialise A.AccountId
instance Serialise SD.TipKey

instance Serialise Ids
instance Serialise Proofs
instance Serialise SBlock
instance Serialise SBlund
instance Serialise Values

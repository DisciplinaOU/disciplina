--  I'm not sure how to structure serialisation instances for now.

module Dscp.Snowdrop.Serialise () where

import Codec.Serialise (Serialise (..))
import qualified Snowdrop.Model.Block as SD
import qualified Snowdrop.Model.State.Accounting.Account as SD
import Snowdrop.Util (ChangeSet (..), ValueOp (..))

import Dscp.Core.Types
import Dscp.Snowdrop.Configuration

deriving instance Generic (ChangeSet id v)
deriving instance Generic (SD.BlockRef a)

instance Serialise v => Serialise (ValueOp v)
instance (Ord id, Serialise id, Serialise v) => Serialise (ChangeSet id v)
instance Serialise a => Serialise (SD.TipValue a)
instance Serialise a => Serialise (SD.BlockRef a)
instance Serialise SD.Account
instance Serialise SD.TipKey
instance Serialise a => Serialise (SD.Author a)

instance Serialise Address
instance Serialise Coin
instance Serialise TxElem
instance Serialise Tx
instance Serialise TxWitness
instance Serialise TxWithWitness
instance Serialise BlockToSign
instance Serialise Difficulty
instance Serialise Header
instance Serialise Block
instance Serialise GlobalTx
instance Serialise BlockBody

instance Serialise Ids
instance Serialise Values
instance Serialise SBlock
instance Serialise SBlund

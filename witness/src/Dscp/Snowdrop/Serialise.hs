--  I'm not sure how to structure serialisation instances for now.

module Dscp.Snowdrop.Serialise () where

import Codec.Serialise (Serialise (..))
import qualified Snowdrop.Block as SD
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Util as SD

import Dscp.Core ()
import Dscp.Crypto (DscpSigScheme)
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Types

instance Serialise (SD.Signature DscpSigScheme msg)
instance Serialise (SD.PublicKey DscpSigScheme)

deriving instance Generic (SD.ChangeSet id v)
deriving instance Generic (SD.WithSignature sigScheme a)
deriving instance Generic (SD.BlockRef a)

instance Serialise v => Serialise (SD.ValueOp v)
instance (Serialise (SD.Signature sigScheme a), Serialise (SD.PublicKey sigScheme), Serialise a) => Serialise (SD.WithSignature sigScheme a)
instance (Ord id, Serialise id, Serialise v) => Serialise (SD.ChangeSet id v)
instance (Ord id, Serialise id, Serialise v) => Serialise (SD.Undo id v)
instance Serialise a => Serialise (SD.TipValue a)
instance Serialise a => Serialise (SD.BlockRef a)
instance Serialise SD.StateTxType
instance (Serialise id, Ord id, Serialise v, Serialise proof) => Serialise (SD.StateTx id proof v)
instance Serialise Account
instance Serialise Author
instance Serialise AccountId
instance Serialise SD.TipKey

instance Serialise Ids
instance Serialise Proofs
instance (Serialise h, Serialise b) => Serialise (SD.Block h b)
instance Serialise SPayload
instance Serialise SBlund
instance Serialise Values

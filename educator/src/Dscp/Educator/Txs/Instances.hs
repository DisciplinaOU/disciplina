module Dscp.Educator.Txs.Instances () where

import Codec.Serialise (Serialise (..))

import Dscp.Core.Serialise ()
import Dscp.Crypto (Hash, hash)
import Dscp.Crypto.Serialise ()
import Dscp.Educator.Txs.Type (PrivateTx (..), PrivateTxAux (..), PrivateTxWitness (..))
import Dscp.Util (HasId (..))

-- TODO: make well-defined Serialise instances instead of generic ones

-- | Transactions
instance Serialise PrivateTx
instance Serialise PrivateTxWitness
instance Serialise PrivateTxAux

instance HasId PrivateTx where
    type Id PrivateTx = Hash PrivateTx
    getId = hash

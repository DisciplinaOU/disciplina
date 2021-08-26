-- | Avl+ related types and instances.

module Dscp.Witness.AVL.Proof
    ( AvlProof
    ) where

import Codec.Serialise (Serialise (..))
import qualified Data.Tree.AVL as AVL
import Snowdrop.Execution ()

import Dscp.Snowdrop.Configuration (Ids, Values)
import Dscp.Witness.AVL.Hash

-- Should it be a newtype?
type AvlProof = AVL.Proof AvlHash Ids Values

instance (Serialise h, Serialise k, Serialise v) => Serialise (AVL.Proof h k v)

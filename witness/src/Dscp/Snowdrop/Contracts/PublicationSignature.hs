
module Dscp.Snowdrop.Contracts.PublicationSignature where

import Dscp.Crypto.MerkleTree
import Dscp.Snowdrop.Contracts.Util

type PublicationSignature = MerkleSignature PrivateTx

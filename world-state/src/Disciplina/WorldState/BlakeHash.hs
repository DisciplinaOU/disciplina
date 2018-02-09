
module Disciplina.WorldState.BlakeHash () where

import Data.Binary
import Data.ByteString
import Crypto.Hash

import qualified Data.Tree.AVL as AVL

data Hash = Hash ByteString

instance (Binary k, Binary v) => AVL.Hash Hash k v where


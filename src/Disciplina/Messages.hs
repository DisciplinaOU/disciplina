module Disciplina.Messages where

import Universum

import Codec.Serialise (Serialise)
import qualified Data.ByteString as BS
import Data.Data (Data)
import Node (Message, formatMessage, messageCode)
import Node.Message.Binary (BinaryP, binaryPacking)

-- | Type for messages from the workers to the witnessListeners.
data PingBlk = PingBlk
deriving instance Generic PingBlk
deriving instance Data PingBlk
deriving instance Show PingBlk
instance Serialise PingBlk
instance Message PingBlk where
    messageCode _ = 0
    formatMessage _ = "PingBlk"

data PingTx = PingTx
deriving instance Generic PingTx
deriving instance Data PingTx
deriving instance Show PingTx
instance Serialise PingTx
instance Message PingTx where
    messageCode _ = 1
    formatMessage _ = "PingTx"

-- | Type for messages from the witnessListeners to the workers.
data PongBlk = PongBlk BS.ByteString
deriving instance Generic PongBlk
deriving instance Show PongBlk
instance Serialise PongBlk

data PongTx = PongTx BS.ByteString
deriving instance Generic PongTx
deriving instance Show PongTx
instance Serialise PongTx

type Packing = BinaryP


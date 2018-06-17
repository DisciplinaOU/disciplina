{-# LANGUAGE DataKinds #-}

-- TODO This module should be probably moved into witness later.

module Dscp.Network.Messages
       ( PingBlk (..)
       , PongBlk (..)
       , PingTx (..)
       , PongTx (..)
       ) where

import Universum

import Codec.Serialise (Serialise)
import Loot.Network.Message (Message (..))

-- | Type for messages from the workers to the witnessListeners.
data PingBlk = PingBlk deriving (Generic,Show)
instance Serialise PingBlk
instance Message PingBlk where type MsgTag PingBlk = 0

data PingTx = PingTx deriving (Generic,Show)
instance Serialise PingTx
instance Message PingTx where type MsgTag PingTx = 1

-- | Type for messages from the witnessListeners to the workers.
data PongBlk = PongBlk ByteString deriving (Generic,Show)
instance Serialise PongBlk

data PongTx = PongTx ByteString deriving (Generic,Show)
instance Serialise PongTx

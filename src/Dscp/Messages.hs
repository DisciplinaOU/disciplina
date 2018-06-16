module Dscp.Messages
       ( PingBlk (..)
       , PongBlk (..)
       , PingTx (..)
       , PongTx (..)
       , Packing
       , serialisePacking
       ) where

import Universum

import Codec.Serialise (IDecode (..), Serialise, deserialiseIncremental, serialise)
import Control.Monad.ST (RealWorld, ST)
import qualified Data.ByteString as BS
import Data.Data (Data)
import Node.Message.Class (Message (..), PackingType (..), Serializable (..))
import qualified Node.Message.Class as Msg
import qualified Node.Message.Decoder as Msg
import System.IO.Unsafe (unsafePerformIO)

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

-- | Pack messages using 'Codec.Serialise'
data SerialiseP

instance PackingType SerialiseP where
    type PackM SerialiseP = Identity
    -- `IO` and not `ST s`, because `s` parameter doesn't get
    -- propagated this way.
    type UnpackM SerialiseP = IO

fromIDecode :: IDecode RealWorld a -> Msg.DecoderStep (UnpackM SerialiseP) a
fromIDecode (Done bs bo res) = Msg.Done bs bo res
fromIDecode (Fail bs bo err) = Msg.Fail bs bo (show err)
fromIDecode (Partial f)      = Msg.Partial $ mkDecoder . f

mkDecoder :: ST RealWorld (IDecode RealWorld a)
          -> Msg.Decoder (UnpackM SerialiseP) a
mkDecoder = Msg.Decoder . fmap fromIDecode . stToIO

instance Serialise a => Serializable SerialiseP a where
    packMsg _ = pure . serialise
    unpackMsg _ = mkDecoder deserialiseIncremental

serialisePacking :: Applicative m => Msg.Packing SerialiseP m
serialisePacking = Msg.Packing
    { Msg.packingType = Proxy
    , Msg.packM = pure . runIdentity
    , Msg.unpackM = pure . unsafePerformIO
    -- ^ Considered to be safe, because 'Decoder' only does
    -- reads of byte arrays.
    }

-- | Choose 'SerialiseP' as default packing mechanism.
type Packing = SerialiseP


-- | Starting point for running a Witness node

module Listeners where

import Universum

import           Data.Binary (Binary)
import Mockable (runProduction, Production (..))
import System.Wlog (logInfo, logWarning)

import Disciplina.Launcher (BasicNodeParams (..), LoggingParams (..), bracketBasicNodeResources,
                            runBasicRealMode)
import Params (WitnessParams (..), getWitnessParams)

import qualified Network.Transport.TCP as TCP
import           Node
import           Data.Data (Data)
import           Node.Message.Binary (BinaryP, binaryPacking)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import           System.Random
import           Mockable.Concurrent (delay, forConcurrently, fork, killThread)
import           Network.Transport.Abstract (closeTransport, Transport)
import           Network.Transport.Concrete (concrete)
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           System.IO (getChar)

import Messages




witnessListeners
    :: NodeId
    -> BS.ByteString
    -> [Listener Packing BS.ByteString Production]
witnessListeners anId peerData = [blkListener, txListener]
    where
    blkListener :: Listener Packing BS.ByteString Production
    blkListener = Listener $ \_ peerId (cactions :: ConversationActions PongBlk PingBlk Production) -> do
        logInfo "heard Blk"
        send cactions (PongBlk "")
    txListener :: Listener Packing BS.ByteString Production
    txListener = Listener $ \_ peerId (cactions :: ConversationActions PongTx PingTx Production) -> do
        logInfo "heard Tx"
        send cactions (PongTx "")

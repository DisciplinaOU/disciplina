
module Disciplina.WorldState.Adapter where

import Universum

import Control.Concurrent
import Control.Concurrent.STM.TChan

import Data.Default

import Disciplina.Utils.Plug
import Disciplina.WorldState.Internal
--import Disciplina.WorldState.BlakeHash

runClient :: Client -> Plug (WithProof Transaction) -> IO (Plug (WithProof Transaction, Client))
runClient client input = do
    output  <- makePlug
    channel <- newTChanIO

    unsub <- input `subscribe` \transaction -> do
        atomically $ do
            channel `writeTChan` transaction

    _ <- forkIO $ do
        res <- try $ do
            runWorldM def client $ do
                forever $ do
                    trans <- liftSTM $ readTChan channel
                    assumeTransaction trans
                    newState <- get
                    liftIO $ output `receive` (trans, newState)

        whenLeft res $ \(SomeException _) -> do
            unsubscribe unsub

    return output

liftSTM :: STM a -> WorldM side a
liftSTM = liftIO . atomically
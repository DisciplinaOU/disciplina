
module Disciplina.Utils.Plug (Plug, makePlug, receive, subscribe, unsubscribe) where

import Universum

--import Control.Concurrent.STM

import qualified Data.IntMap as IntMap

--import Disciplina.WorldState.Internal
--import Disciplina.WorldState.BlakeHash

data Plug a = Plug
    { pReceive   ::  a -> IO ()
    , pSubscribe :: (a -> IO ()) -> IO Unsubscribe
    }

receive :: Plug a -> a -> IO ()
receive   = pReceive

subscribe :: Plug a -> (a -> IO ()) -> IO Unsubscribe
subscribe = pSubscribe

data Unsubscribe = Unsubscribe { uUnsubscribe :: IO () }

unsubscribe :: Unsubscribe -> IO ()
unsubscribe = uUnsubscribe

makePlug :: IO (Plug a)
makePlug = do
    subs <- newTVarIO (0, IntMap.empty)

    let
      pReceive a = do
        (_, subscribers) <- atomically $ readTVar subs
        IntMap.elems subscribers `for_` \consume -> do
            consume a

      pSubscribe consumer = do
        index <- atomically $ do
            (last, subscribers) <- readTVar subs
            subs `writeTVar` (last + 1, IntMap.insert last consumer subscribers)
            return last

        return $ Unsubscribe $ atomically $ do
            (last, subscribers) <- readTVar subs
            subs `writeTVar` (last, IntMap.delete index subscribers)

    return (Plug { pReceive, pSubscribe })
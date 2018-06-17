module Disciplina.DB.Class where

import Universum

-- | Read-only interface to key-value DB storing serialized data.
class Monad m => MonadDBRead m where
    dbGet :: ByteString -> m (Maybe ByteString)

-- | Full interface to key-value DB.
class MonadDBRead m => MonadDB m where
    dbPut :: ByteString -> ByteString -> m ()
    dbDelete :: ByteString -> m ()

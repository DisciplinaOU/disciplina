
{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.Schema (ensureSchemaIsSetUp) where

import Database.SQLite.Simple.Internal (Connection (..))
import Database.SQLite3 (exec)

import Dscp.DB.SQLite.FileQuoter (qFile)

ensureSchemaIsSetUp :: MonadIO m => Connection -> m ()
ensureSchemaIsSetUp (Connection db) = do
    liftIO $ exec db schema

schema :: IsString s => s
schema = [qFile|./database/schema.sql|]

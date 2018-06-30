
{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.Schema (ensureSchemaIsSetUp) where

import Database.SQLite3 (exec)
import Database.SQLite.Simple.Internal (Connection (..))

import Dscp.DB.SQLite.FileQuoter (qFile)

ensureSchemaIsSetUp :: Connection -> IO ()
ensureSchemaIsSetUp (Connection db) = do
    exec db schema

schema :: IsString s => s
schema = [qFile|./database/schema.sql|]

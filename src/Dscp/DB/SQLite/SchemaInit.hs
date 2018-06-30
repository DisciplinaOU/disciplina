
{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.SchemaInit
    ( ensureSchemaIsSetUp
    ) where

import Database.SQLite3 (exec)
import Database.SQLite.Simple.Internal (Connection (..))

import Dscp.DB.SQLite.Schema (schema)

ensureSchemaIsSetUp :: Connection -> IO ()
ensureSchemaIsSetUp (Connection db) = do
    exec db schema


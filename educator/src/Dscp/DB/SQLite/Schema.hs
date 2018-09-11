
{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.Schema
    ( ensureSchemaIsSetUp
    , applySchemaSettings
    ) where

import Database.SQLite.Simple.Internal (Connection (..))
import Database.SQLite3 (exec)

import Dscp.DB.SQLite.FileQuoter (qFile)

-- | Create tables if absent.
ensureSchemaIsSetUp :: MonadIO m => Connection -> m ()
ensureSchemaIsSetUp (Connection db) = do
    liftIO $ exec db schema

-- | Apply schema settings, should be invoked for every new connection.
applySchemaSettings :: MonadIO m => Connection -> m ()
applySchemaSettings (Connection db) =
    liftIO $ exec db schemaSettings

schema :: IsString s => s
schema = [qFile|./database/schema.sql|]

schemaSettings :: IsString s => s
schemaSettings = [qFile|./database/settings.sql|]

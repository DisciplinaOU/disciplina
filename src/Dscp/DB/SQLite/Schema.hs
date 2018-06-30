
{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.Schema (schema) where

import Universum

import Dscp.DB.SQLite.FileQuoter (qFile)

schema :: IsString s => s
schema = [qFile|./database/schema.sql|]

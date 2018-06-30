
{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.DSL.Interpret.Sqlite3.Schema (schema) where

import Universum

import Text.InterpolatedString.Perl6 (q)

import Dscp.DB.DSL.Interpret.Sqlite3.FileQuoter (qFile)

schema :: IsString s => s
schema =
    [qFile|./database/schema.sql|]

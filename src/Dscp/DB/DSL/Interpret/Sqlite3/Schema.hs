
{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.DSL.Interpret.Sqlite3.Schema (schema) where

import Universum

import Text.InterpolatedString.Perl6 (q)

import Dscp.DB.DSL.Interpret.Sqlite3.FileQuoter (qFile)

schema :: IsString s => s
schema =
    -- Spaces between quasiquote brackets and file name actually break lookup.
    -- DO NOT believe documentation example at:
    -- http://hackage.haskell.org/package/template-haskell-2.7.0.0/docs/Language-Haskell-TH-Quote.html
    [qFile|./database/schema.sql|]

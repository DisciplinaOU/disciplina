module Dscp.DB.SQLite.FileQuoter (qFile) where

import Language.Haskell.TH.Quote (quoteFile, QuasiQuoter)

import Text.InterpolatedString.Perl6 (q)

{-
    Was placed in separate file due to this:

    • GHC stage restriction:
        ‘qFile’ is used in a top-level splice, quasi-quote, or annotation,
        and must be imported, not defined locally
-}

qFile :: QuasiQuoter
qFile = quoteFile q

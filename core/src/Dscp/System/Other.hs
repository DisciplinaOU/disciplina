
module Dscp.System.Other
    (
      -- * Application
      appName

      -- * Git
    , gitInfo

      -- * OS-dependent operations
    , IsPosix
    , whenPosix
    ) where

import Universum
import Data.Reflection (Given, give)
import qualified Development.GitRev as Git
import Fmt ((+|))
import System.Info (os)

---------------------------------------------------------------------
-- Application
---------------------------------------------------------------------

-- | Name of this application.
appName :: IsString s => s
appName = "disciplina"
{-# SPECIALIZE appName :: String #-}
{-# SPECIALIZE appName :: Text #-}

---------------------------------------------------------------------
-- Git
---------------------------------------------------------------------

-- | Human-readable description of git revision which this project is built with.
--
-- This variable can show outdated info until the module containing it is recompiled,
-- i.e. change of git revision does not force recompilation on itself.
-- Thus in development builds produced text may be incorrect.
gitInfo :: IsString s => s
gitInfo = fromString $
    "Git revision: " +| $(Git.gitCommitCount) +| "-th commit at "
                     +| $(Git.gitBranch) +| " branch - "
                     +| $(Git.gitCommitDate) +| " - "
                     +| $(Git.gitDescribe)

---------------------------------------------------------------------
-- OS-dependent operations
---------------------------------------------------------------------

data PosixOperationsEnabled = PosixOperationsEnabled

-- | Allowance for POSIX operations.
type IsPosix = Given PosixOperationsEnabled

-- | Perform an action only on POSIX systems.
whenPosix :: Monad m => (IsPosix => m ()) -> m ()
whenPosix = case os of
    "windows" -> \_ -> pass
    _         -> give PosixOperationsEnabled

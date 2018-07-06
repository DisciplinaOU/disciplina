
module Dscp.System.Other
    (
      -- * Application
      appName

      -- * OS-dependent operations
    , IsPosix
    , whenPosix
    ) where

import Data.Reflection (Given, give)
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

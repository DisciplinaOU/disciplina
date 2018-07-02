-- | Interaction with OS.

module Dscp.System
    (
      -- * Application
      appName
    ) where

---------------------------------------------------------------------
-- Application
---------------------------------------------------------------------

-- | Name of this application.
appName :: IsString s => s
appName = "disciplina"
{-# SPECIALIZE appName :: String #-}
{-# SPECIALIZE appName :: Text #-}


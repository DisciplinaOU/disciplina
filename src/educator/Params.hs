
-- | Command-line options and flags for Educator node

module Params where

import Data.Version (showVersion)
import Options.Applicative (Parser, execParser, fullDesc, help, helper, info, infoOption, long,
                            progDesc)

{-# LANGUAGE ApplicativeDo #-}

-- | CLI for educator.

module Dscp.Educator.CLI
    ( educatorParamsParser
    ) where

import Options.Applicative (Parser)

import Dscp.CLI (sqliteParamsParser)
import Dscp.Crypto (mkPassPhrase)
import Dscp.Educator.Launcher.Params (EducatorParams (..))
import Dscp.Educator.Secret (EducatorSecretParams (..))
import Dscp.Witness.CLI (witnessParamsParser)

educatorSecretParser :: Parser EducatorSecretParams
educatorSecretParser = do
    -- TODO [DSCP-120]: implement
    espPath <- pure Nothing
    espGenNew <- pure False
    espPassphrase <- pure $ either (error "Mem") identity $ mkPassPhrase "NyanForever"
    return EducatorSecretParams{..}

educatorParamsParser :: Parser EducatorParams
educatorParamsParser = do
    epWitnessParams <- witnessParamsParser
    epDBParams <- sqliteParamsParser
    epSecretParams <- educatorSecretParser
    return EducatorParams{..}

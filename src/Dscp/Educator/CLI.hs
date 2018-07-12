{-# LANGUAGE ApplicativeDo #-}

-- | CLI for educator.

module Dscp.Educator.CLI
    ( educatorParamsParser
    ) where

import Options.Applicative (Parser, ReadM, help, long, metavar, option, optional, str, strOption,
                            switch)

import Dscp.CLI (networkAddressParser, sqliteParamsParser)
import Dscp.Crypto (PassPhrase, mkPassPhrase)
import Dscp.Educator.Launcher.Params (EducatorParams (..))
import Dscp.Educator.Secret (EducatorSecretParams (..))
import Dscp.Educator.Web.Params (EducatorWebParams (..))
import Dscp.Util (leftToFail)
import Dscp.Witness.CLI (witnessParamsParser)

passphraseReadM :: ReadM PassPhrase
passphraseReadM = leftToFail . first pretty . mkPassPhrase =<< str

educatorSecretParamsParser :: Parser EducatorSecretParams
educatorSecretParamsParser = do
    espPath <- optional . strOption $
         long "keyfile-path" <>
         metavar "FILEPATH" <>
         help "Path to file with educator secret key."
    espGenNew <- switch $
         long "generate-new-key" <>
         help "Create new keyfile instead of using existing one."
    espPassphrase <- option passphraseReadM $
         long "keyfile-password" <>
         metavar "PASSWORD" <>
         help "Password from secret key."
    return EducatorSecretParams{..}

educatorWebParamsParser :: Parser EducatorWebParams
educatorWebParamsParser = do
    ewpStudentApiAddr <- networkAddressParser "student-listen"
        "Host/port for serving Student API"
    return EducatorWebParams{..}

educatorParamsParser :: Parser EducatorParams
educatorParamsParser = do
    epWitnessParams <- witnessParamsParser
    epDBParams <- sqliteParamsParser
    epSecretParams <- educatorSecretParamsParser
    epWebParams <- educatorWebParamsParser
    return EducatorParams{..}

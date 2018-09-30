{-# LANGUAGE ApplicativeDo #-}

module KeygenOptions
       ( KeygenConfig(..)
       , keygenConfigParser
       ) where

import Options.Applicative (Parser, ReadM, auto, flag', help, long, metavar, option, str)

import Dscp.CommonCLI
import Dscp.Util

import KeygenCommands
import KeygenSecret

keygenCommandReadM :: ReadM KeygenCommand
keygenCommandReadM = leftToFail . parseKeygenCommand =<< str

-- | Parser for all possble ways to give secret key.
secretDataTypeParser :: Parser SecretDataType
secretDataTypeParser = asum
    [ flag' ()
        (long "secret" <>
         help "Consume secret key (maybe encrypted)")
      *> (PlainSecret <$> passphraseParser)

    , flag' ()
        (long "keyfile" <>
         help "Consume key file content")
      *> (KeyfileSecret <$> passphraseParser)

    , CommSecret <$> option auto
        (long "comm-sec" <>
         metavar "N" <>
         help "Use given committee secret and derive secret key of N-th \
              \committee member from it.")

    , flag' ()
        (long "seed" <>
         help "Use given seed to generate base secret.")
      $> SecretFromSeed
    ]
  where
    passphraseParser = optional . option passphraseReadM $
        long "password" <>
        metavar "TEXT" <>
        help "Password from given key passed in input. If no password is \
             \specified, secret key is assumed to have no password."

keygenCommandParser :: Parser KeygenCommand
keygenCommandParser = option keygenCommandReadM $
    long "command" <>
    metavar "TEXT" <>
    help "Which output should be produced. See README.md for details"

-- | All parameters.
data KeygenConfig = KeygenConfig
    { kcSecretDataType :: !SecretDataType
    , kcKeygenCommand  :: !KeygenCommand
    }

-- | Parse all parameters.
keygenConfigParser :: Parser KeygenConfig
keygenConfigParser = do
    kcSecretDataType <- secretDataTypeParser
    kcKeygenCommand <- keygenCommandParser
    return KeygenConfig{..}

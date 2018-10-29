{-# LANGUAGE ApplicativeDo #-}

module KeygenOptions
       ( KeygenConfig(..)
       , keygenConfigParser
       ) where

import Options.Applicative (Parser, auto, command, flag', fullDesc, help, hsubparser, info, long,
                            metavar, noIntersperse, option, progDesc, short, strArgument, strOption,
                            value)

import Dscp.CommonCLI
import Dscp.Crypto
import Dscp.Util

import KeygenCommands
import KeygenSecret

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
        short 'p' <>
        metavar "TEXT" <>
        help "Password from the key passed in input."

viewParser :: Parser (Maybe View)
viewParser = optional $ asum
    [ flag' RawView $
          long "raw" <>
          help "Display output as is"
    , flag' HexView $
          long "hex" <>
          help "Display output in hex"
    , flag' HexView $
          long "base16" <>
          help "Display output in hex"
    , flag' Base64View $
          long "base64" <>
          help "Display output in base64"
    ]

prettinessParser :: Parser (Maybe Pretty)
prettinessParser = optional $ asum
    [ flag' (Pretty True) $
          long "pretty" <>
          help "Format output in pretty way"
    , flag' (Pretty False) $
          long "one-line" <>
          help "Print output in one line"
    ]

signedEndpointNameParser :: Parser Text
signedEndpointNameParser = strArgument $
    metavar "ENDPOINT" <>
    help "Exact name of the signed endpoint."

submissionSeedParser :: Parser (Seed Text)
submissionSeedParser = strOption $
    long "sub-seed" <>
    metavar "TEXT" <>
    help "Seed used to generate a submission."

keygenCommandParser :: Parser KeygenCommand
keygenCommandParser = hsubparser $ mconcat
    [ command "secret" $ info
        (PrintSecretKey . fromMaybe Base64View <$> viewParser)
        (fullDesc <>
         progDesc "Display the secret itself. In base64 by default.")

    , command "public" $ info
        (PrintPublicKey . fromMaybe HexView <$> viewParser)
        (fullDesc <>
         progDesc "Display the public key corresponding to the secret. In hex by default.")

    , command "address" $ info
        (pure PrintAddress)
        (fullDesc <>
         progDesc "Display the address derivative from the secret.")

    , command "esecret" $ info
        (PrintEncryptedSecretKey <$> passphraseParser <*> (viewParser <?:> Base64View))
        (fullDesc <>
         noIntersperse <>
         -- needed since --password may be both an option or a command argument otherwise
         progDesc "Display secret key encrypted. Note that if no password is given, an \
                  \encrypted secret will be produced anyway (which is ~twice as long as \
                  \an unencrypted secret.")

    , command "keyfile" $ info
        (PrintKeyFile <$> passphraseParser <*> (prettinessParser <?:> Pretty True))
        (fullDesc <>
         noIntersperse <>
         progDesc "Display keyfile content. You will probably want to redirect the output \
                  \of this command to the file.")

    , command "educator-auth" $ info
        (PrintEducatorAuthToken <$> signedEndpointNameParser)
        (fullDesc <>
         progDesc "Create a JWT signature for authentication in Educator/Student API. \
                  \You have to provide the name of signed endpoint, for instance \
                  \`/api/student/v1/submissions`")

    , command "student-submission" $ info
        (PrintStudentSubmission <$> submissionSeedParser)
        (fullDesc <>
         progDesc "Generate a submission in JSON format which can be put to \
                  \the `POST /submissions` endpoint of Student API.")
    ]
  where
    passphraseParser = option passphraseReadM $
        long "password" <>
        short 'p' <>
        metavar "TEXT" <>
        help "Password to encrypt the secret with." <>
        value emptyPassPhrase

-- | All parameters.
data KeygenConfig = KeygenConfig
    { kcSecretDataType :: !SecretDataType
    , kcKeygenCommand  :: !KeygenCommand
    } deriving (Show)

-- | Parse all parameters.
keygenConfigParser :: Parser KeygenConfig
keygenConfigParser = do
    kcSecretDataType <- secretDataTypeParser
    kcKeygenCommand <- keygenCommandParser
    return KeygenConfig{..}

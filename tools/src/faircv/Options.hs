module Options
       ( FaircvOptions(..)
       , getFaircvOptions
       ) where

import Options.Applicative (Parser, auto, execParser, fullDesc, help, helper,
                            info, long, metavar, option, progDesc, short,
                            showDefault, strOption, value)
import Time.Rational (RatioNat)


data FaircvOptions = FaircvOptions
    { witnessUrl    :: String
    , educatorUrl   :: String
    , secretKeyFile :: FilePath
    , refreshRate   :: RatioNat
    , studentSeed   :: ByteString
    }

getFaircvOptions :: IO FaircvOptions
getFaircvOptions = execParser $ info (helper <*> optionParser) $
    fullDesc <> progDesc "Disciplina faircv generator"

optionParser :: Parser FaircvOptions
optionParser = FaircvOptions
    <$> witnessUrlParser
    <*> educatorUrlParser
    <*> secretKeyFileParser
    <*> refreshRateParser
    <*> studentSecretSeedParser

witnessUrlParser :: Parser String
witnessUrlParser = strOption $
    long "witness-url" <>
    short 'w' <>
    metavar "URL" <>
    help "Url for the witness API."

educatorUrlParser :: Parser String
educatorUrlParser = strOption $
    long "educator-url" <>
    short 'e' <>
    metavar "URL" <>
    help "Url for the student API."

secretKeyFileParser :: Parser FilePath
secretKeyFileParser = strOption $
    long "key-file" <>
    short 'k' <>
    metavar "PATH" <>
    help "Path for the educator secret key file"

refreshRateParser :: Parser RatioNat
refreshRateParser = option auto $
    long "refresh-rate" <>
    short 'r' <>
    metavar "INT" <>
    value 3 <>
    showDefault <>
    help "Time to wait between a request and another, in seconds"

studentSecretSeedParser :: Parser ByteString
studentSecretSeedParser = option auto $
    long "student-seed" <>
    short 's' <>
    value "456" <>
    showDefault <>
    help "Seed for the student private key, see keygen for more info"

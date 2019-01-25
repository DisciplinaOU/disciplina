module FaircvOptions
       ( FaircvOptions(..)
       , getFaircvOptions
       ) where

import Options.Applicative (Parser, auto, execParser, fullDesc, help, helper,
                            info, long, metavar, option, progDesc, short,
                            showDefault, strOption, value)
import Time.Units (Time, Second, sec)

data FaircvOptions = FaircvOptions
    { witnessUrl    :: String
    , educatorUrl   :: String
    , secretKeyFile :: FilePath
    , refreshRate   :: (Time Second)
    , studentSeed   :: ByteString
    , assignmentNum :: Int
    , outputFile    :: FilePath
    , contentSeed   :: Text
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
    <*> assignmentNumberParser
    <*> outputFileParser
    <*> contentSeedParser

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

refreshRateParser :: Parser (Time Second)
refreshRateParser = sec <$> option auto (
    long "refresh-rate" <>
    short 'r' <>
    value 3 <>
    showDefault <>
    help "Time to wait between a proof request and another, in seconds"
    )

studentSecretSeedParser :: Parser ByteString
studentSecretSeedParser = option auto $
    long "student-seed" <>
    short 's' <>
    value "456" <>
    showDefault <>
    help "Seed for the student private key, see keygen for more info"

assignmentNumberParser :: Parser Int
assignmentNumberParser = option auto $
    long "assignments" <>
    short 'n' <>
    value 3 <>
    showDefault <>
    metavar "INT" <>
    help "Number of assignment to include in the FairCV"

outputFileParser :: Parser FilePath
outputFileParser = strOption $
    long "file-output" <>
    short 'f' <>
    value "fairCV-example.json" <>
    showDefault <>
    metavar "PATH" <>
    help "File that will contain the resulting FairCV"

contentSeedParser :: Parser Text
contentSeedParser = option auto $
    long "content-seed" <>
    short 'c' <>
    value "54321" <>
    metavar "TEXT" <>
    help "Seed used to generate the assignment's content hash"

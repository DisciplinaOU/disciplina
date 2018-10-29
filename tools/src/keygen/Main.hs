import qualified Data.ByteString as BS

import Data.Word8 (isSpace)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Dscp.CommonCLI

import KeygenCommands
import KeygenOptions
import KeygenSecret

main :: IO ()
main = do
    KeygenConfig inputType command <- getKeygenConfig

    rawInput <- BS.getContents
    let (input, _) = BS.spanEnd isSpace rawInput
    let !secret = parseInputWithSecret inputType input
               ?: error "Cannot parse input"

    let output = keygenCommandExecutor secret command
    putTextLn output

getKeygenConfig :: IO KeygenConfig
getKeygenConfig =
    execParser $
        info (helper <*> versionOption <*> keygenConfigParser) $
        fullDesc <>
        progDesc "Disciplina keygen. Can be used for producing a secret key \
                 \and its derivative data."

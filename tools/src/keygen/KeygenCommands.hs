module KeygenCommands
    ( KeygenCommand
    , parseKeygenCommand
    , keygenCommandExecutor
    ) where

import Prelude hiding (view)

import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteArray as BA
import qualified Data.Text as T

import Dscp.Core
import Dscp.Crypto
import Dscp.Educator.Web.Auth
import Dscp.Resource.Keys
import Dscp.Util
import Dscp.Util.Aeson
import Dscp.Util.Serialise

-- | How to output bytestring-like data.
data View
    = RawView
    | HexView
    | Base64View

-- | Parse 'View' from command option.
readView :: Text -> Either Text View
readView = \case
    "raw" -> pure RawView
    "base64" -> pure Base64View
    "base16" -> pure HexView
    "hex" -> pure HexView
    other -> Left $ "Unknown view option: " <> other

-- | Do conversion according to 'View'.
viewBS :: View -> ByteString -> Text
viewBS = \case
    RawView -> decodeUtf8
    HexView -> toHex
    Base64View -> toBase64

-- | Parse 'PassPhrase' from command option.
readPassPhrase :: Text -> Either Text PassPhrase
readPassPhrase "" = Right emptyPassPhrase
readPassPhrase pp = first show . mkPassPhrase $ encodeUtf8 pp

-- | Whether to use pretty multiline output.
newtype Pretty = Pretty Bool

readPrettyOption :: Text -> Either Text Pretty
readPrettyOption = \case
    "pretty" -> pure $ Pretty True
    "one-line" -> pure $ Pretty False
    other -> Left $ "Unknown prettiness option: " <> other

-- | All commands keygen supports.
data KeygenCommand
    = PrintSecretKey View
    | PrintPublicKey View
    | PrintAddress
    | PrintEncryptedSecretKey PassPhrase View
    | PrintKeyFile PassPhrase Pretty
    | PrintEducatorAuthToken Text

-- | Parse a command.
parseKeygenCommand :: Text -> Either Text KeygenCommand
parseKeygenCommand command =
    case T.splitOn ":" command of
        "secret" : [] ->
            return $ PrintSecretKey Base64View
        "secret" : [viewOpt] -> do
            view <- readView viewOpt
            return $ PrintSecretKey view

        "public" : [] ->
            return $ PrintPublicKey HexView
        "public" : [viewOpt] -> do
            view <- readView viewOpt
            return $ PrintSecretKey view

        "address" : [] ->
            return PrintAddress

        "esecret" : [passOpt, viewOpt] -> do
            pp <- readPassPhrase passOpt
            view <- readView viewOpt
            return $ PrintEncryptedSecretKey pp view
        "esecret" : [passOpt] -> do
            pp <- readPassPhrase passOpt
            return $ PrintEncryptedSecretKey pp Base64View
        "esecret" : [] -> do
            return $ PrintEncryptedSecretKey emptyPassPhrase Base64View

        "keyfile" : [] ->
            return $ PrintKeyFile emptyPassPhrase (Pretty True)
        "keyfile" : [passOpt] -> do
            pp <- readPassPhrase passOpt
            return $ PrintKeyFile pp (Pretty True)
        "keyfile" : [passOpt, prettyOpt] -> do
            pp <- readPassPhrase passOpt
            pr <- readPrettyOption prettyOpt
            return $ PrintKeyFile pp pr

        "educator-auth" : [] ->
            Left "'educator-auth' command expects endpoint name as argument"
        "educator-auth" : [endpointName] ->
            return $ PrintEducatorAuthToken endpointName

        _ -> Left $ "Unknown command: " <> command

-- | Produce output following the given command.
keygenCommandExecutor :: SecretKey -> KeygenCommand -> Text
keygenCommandExecutor secret = \case
    PrintSecretKey view ->
        viewBS view $ BA.convert secret
    PrintPublicKey view ->
        viewBS view (BA.convert $ toPublic secret)
    PrintAddress ->
        addrToText . mkAddr $ toPublic secret
    PrintEncryptedSecretKey pp view ->
        viewBS view . serialise' $ encrypt pp secret
    PrintKeyFile pp (Pretty pr) ->
        let doEncode = if pr
                       then encodePretty @KeyfileContent
                       else encode @KeyfileContent
        in decodeUtf8 . doEncode $
           Versioned $ toSecretJson pp secret
    PrintEducatorAuthToken endpointName ->
        makeAuthToken secret endpointName

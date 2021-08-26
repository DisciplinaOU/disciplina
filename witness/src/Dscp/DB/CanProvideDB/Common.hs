
module Dscp.DB.CanProvideDB.Common
    ( deserialiseErr
    , prefixName
    , idToKey
    )
    where

import Codec.Serialise
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Dscp.Util.Serialise (serialise')

deserialiseErr :: Serialise x => Text -> ByteString -> x
deserialiseErr t b =
    case deserialiseOrFail (LBS.fromStrict b) of
        Left e ->
            error $ "failed to deserialise value for " <>
            t <> ": " <> show e <> ", value: " <> show b
        Right x -> x

-- Maybe we'll want to have distinct rocks prefixes later
prefixName :: Int -> ByteString
prefixName i =
    if i < 0 || i >= fromIntegral (maxBound @Word16)
    then error "rocksPrefix: incorrect"
    else BS.pack [fromIntegral i, 57] -- key + '/'

idToKey :: Serialise k => Int -> k -> ByteString
idToKey i k = prefixName i <> serialise' k

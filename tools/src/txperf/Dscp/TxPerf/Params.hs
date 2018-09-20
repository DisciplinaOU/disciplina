module Dscp.TxPerf.Params where

import Data.Aeson (FromJSON (..), eitherDecode, withText)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON)
import qualified Data.ByteString.Lazy as LBS
import Options.Applicative (execParser, fullDesc, help, helper, info, long, metavar, progDesc,
                            strOption)
import Servant.Client (BaseUrl)

import Dscp.CommonCLI
import Dscp.Crypto
import Dscp.TxPerf.Account

data Params = Params
    { paramWitness :: BaseUrl
    , paramAccKeys :: [SecretKey]
    , paramGenAccs :: Int
    , paramTxCount :: Int
    , paramTxAsync :: Bool
    }

instance FromJSON SecretKey where
    parseJSON = withText "secret key" $
        return . textToSk

deriveFromJSON defaultOptions ''Params

getCLIParams :: IO Params
getCLIParams = do
    let parser = strOption $
            long "config" <>
            metavar "FILEPATH" <>
            help "Path to JSON config."

    configFile <- execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "An utility to flood witness node with valid transactions."

    LBS.readFile configFile >>= either fail return . eitherDecode

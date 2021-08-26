module Dscp.Wallet.KeyStorage
       ( getAccounts
       , addAccount
       ) where

import Data.Aeson (eitherDecode, encode)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Dscp.Core (mkAddr)
import Dscp.Util.Aeson (Versioned (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath.Posix ((</>))

import qualified Data.ByteString.Lazy as LBS

import Dscp.Resource.AppDir
import Dscp.Util.Aeson
import Dscp.Wallet.Face

data Storage = Storage
    { accounts :: [StorageAccount]
    }

data StorageAccount = StorageAccount
    { name      :: Maybe Text
    , secretKey :: EncodeSerialised Base64Encoded (Encrypted SecretKey)
    , publicKey :: PublicKey
    }

deriveJSON defaultOptions ''Storage
deriveJSON defaultOptions ''StorageAccount

getDataDir :: IO FilePath
getDataDir = do
    dir <- getOSAppDir
    createDirectoryIfMissing True dir
    return dir

getStoragePath :: IO FilePath
getStoragePath = getDataDir >>= return . (</> "wallet.json")

getStorageLockPath :: IO FilePath
getStorageLockPath = getDataDir >>= return . (</> "wallet.json.lock")

getAccounts :: IO [Account]
getAccounts = do
    storage <- readStorage
    return $ toAccount <$> accounts storage
  where
    toAccount StorageAccount{..} = Account
        { accountName = name
        , accountSecretKey = unEncodeSerialised secretKey
        , accountPublicKey = publicKey
        , accountAddress = mkAddr publicKey
        }

addAccount :: Account -> IO ()
addAccount account = modifyStorage (\(Storage accs) -> Storage $ accs ++ [fromAccount account])
  where
    fromAccount Account{..} = StorageAccount
        { name = accountName
        , secretKey = EncodeSerialised accountSecretKey
        , publicKey = accountPublicKey
        }

readOrCreateStorage :: IO Storage
readOrCreateStorage = do
    exists <- doesFileExist =<< getStoragePath
    if exists
        then getStoragePath >>= LBS.readFile >>= either fail (\(Versioned a) -> return a) . eitherDecode
        else return $ Storage []

readStorage :: IO Storage
readStorage = do
    storageLockPath <- getStorageLockPath
    withFileLock storageLockPath Shared $ \_ -> readOrCreateStorage

modifyStorage :: (Storage -> Storage) -> IO ()
modifyStorage f = do
    storageLockPath <- getStorageLockPath
    withFileLock storageLockPath Exclusive $ \_ -> do
        storage <- readOrCreateStorage
        storagePath <- getStoragePath
        LBS.writeFile storagePath . encode . Versioned $ f storage

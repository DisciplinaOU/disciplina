module Dscp.Wallet.KeyStorage
       ( getAccounts
       , addAccount
       ) where

import Data.Aeson (eitherDecode, encode)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Dscp.Core (mkAddr)
import Dscp.Util.Aeson (Versioned(..))
import System.Directory (doesFileExist)
import System.FileLock (SharedExclusive (..), withFileLock)

import qualified Data.ByteString.Lazy as LBS

import Dscp.Wallet.Face

data Storage = Storage
    { accounts :: [StorageAccount]
    }

data StorageAccount = StorageAccount
    { secretKey :: Encrypted SecretKey
    , publicKey :: PublicKey
    }

deriveJSON defaultOptions ''Storage
deriveJSON defaultOptions ''StorageAccount

storageFileName :: FilePath
storageFileName = "run/tmp/wallet.json"

storageLockFileName :: FilePath
storageLockFileName = "run/tmp/wallet.json.lock"

getAccounts :: IO [Account]
getAccounts = do
    storage <- readStorage
    return $ toAccount <$> accounts storage
  where
    toAccount StorageAccount{..} = Account
        { accountSecretKey = secretKey
        , accountPublicKey = publicKey
        , accountAddress = mkAddr publicKey
        }

addAccount :: Account -> IO ()
addAccount account = modifyStorage (\(Storage accs) -> Storage $ accs ++ [fromAccount account])
  where
    fromAccount Account{..} = StorageAccount
        { secretKey = accountSecretKey
        , publicKey = accountPublicKey
        }

readOrCreateStorage :: IO Storage
readOrCreateStorage = do
    exists <- doesFileExist storageFileName
    if exists
        then LBS.readFile storageFileName >>= either fail (\(Versioned a) -> return a) . eitherDecode
        else return $ Storage []

readStorage :: IO Storage
readStorage = withFileLock storageLockFileName Shared $ \_ -> readOrCreateStorage

modifyStorage :: (Storage -> Storage) -> IO ()
modifyStorage f = withFileLock storageLockFileName Exclusive $ \_ -> do
    storage <- readOrCreateStorage
    LBS.writeFile storageFileName . encode . Versioned $ f storage
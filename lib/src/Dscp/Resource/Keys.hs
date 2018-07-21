-- Later this file will also present code related to encrypted keys
-- management. It will also provide possibility to choose a key from
-- the predefined test genesis instead of providing a key explicitly.

-- | Key management resource.

module Dscp.Resource.Keys
    ( KeyParams (..)
    , KeyResources (..)
    , krSecretKey
    , krPublicKey
    , ourSecretKey
    , ourPublicKey
    ) where

import qualified Codec.Serialise as S
import Control.Exception (Exception)
import Control.Lens (makeLenses)
import qualified Data.ByteString.Lazy as BSL
import Fmt ((+||), (||+))
import Loot.Base.HasLens (HasLens', lensOf)
import Loot.Log (logDebug, logError, logInfo)
import System.Directory as D
import qualified System.FilePath as FP

import Dscp.Crypto (PublicKey, SecretKey, keyGen, toPublic)
import Dscp.Resource.Class (AllocResource (..), buildComponentR)

-- Password also (later).
-- | Parameters needed for key initialisation.
data KeyParams = KeyParams
    { kpKeyPath :: FilePath
      -- ^ Path to the key file.
    , kpGenKey  :: Bool
      -- ^ If key on the 'kpKeyPath' is not present, generate it and write it.
    } deriving (Eq,Ord,Show)

-- In future this will contain encrypted key (maybe).
-- | Resources related to key management.
data KeyResources = KeyResources
    { _krSecretKey :: SecretKey
      -- ^ Node secret key.
    , _krPublicKey :: PublicKey
      -- ^ Corresponding public key, must be exactly equal to 'toPublic sk'.
    } deriving (Eq,Show)

makeLenses ''KeyResources

ourSecretKey :: (HasLens' ctx KeyResources, MonadReader ctx m) => m SecretKey
ourSecretKey = view $ lensOf @KeyResources . krSecretKey

ourPublicKey :: (HasLens' ctx KeyResources, MonadReader ctx m) => m PublicKey
ourPublicKey = view $ lensOf @KeyResources . krPublicKey

data KeyInitException
    = KeyParseException String
    | CantReadKeyException
    deriving (Eq, Show, Generic)
instance Exception KeyInitException

instance AllocResource KeyParams KeyResources where
    allocResource KeyParams {..} =
        buildComponentR "keys" allocate (const pass)
      where
        allocate = do
            let readKey = do
                    logDebug $ "Reading key from: " +|| kpKeyPath ||+ ""
                    sk <- either (throwM . KeyParseException . show) pure =<<
                          (S.deserialiseOrFail <$> liftIO (BSL.readFile kpKeyPath))
                    pure $ KeyResources sk (toPublic sk)
            let genKey = liftIO $ do
                    (sk,pk) <- keyGen
                    D.createDirectoryIfMissing True (FP.takeDirectory kpKeyPath)
                    BSL.writeFile kpKeyPath (S.serialise sk)
                    pure $ KeyResources sk pk
            fileExists <- liftIO $ D.doesFileExist kpKeyPath
            if | fileExists -> readKey
               | kpGenKey -> do
                   logInfo $ "No key found, generating: " +|| kpKeyPath ||+ ""
                   genKey
               | otherwise -> do
                   logError $ "No key was found, generation was " <>
                              "not requested, on path: " +|| kpKeyPath ||+ ""
                   throwM CantReadKeyException

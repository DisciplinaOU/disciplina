-- | Functions to work with key storage.
-- Later this file will also provide possibility to choose a key from
-- the predefined test genesis instead of providing a key explicitly.

module Dscp.Resource.Keys.Functions
    ( linkStore
    ) where

import Data.Aeson (eitherDecode', encode)
import qualified Data.ByteString.Lazy as LBS
import Fmt ((+||), (|+), (||+))
import Loot.Log (MonadLogging, logDebug, logInfo)
import System.Directory (doesFileExist)
import System.Directory as D
import System.FilePath ((</>))
import qualified System.FilePath as FP

import Dscp.Config (BaseConfig (..), HasBaseConfig, baseConfig)
import Dscp.Crypto (PassPhrase, decrypt, encrypt, keyGen, runSecureRandom, toPublic)
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.Resource.Keys.Error (KeyInitError (..), rewrapKeyIOErrors)
import Dscp.Resource.Keys.Types (KeyJson (..), KeyParams (..), KeyResources (..), KeyfileContent)
import Dscp.System (ensureModeIs, mode600, setMode, whenPosix)
import Dscp.Util (leftToThrow)
import Dscp.Util.Aeson (Versioned (..))

---------------------------------------------------------------------
-- Conversions
---------------------------------------------------------------------

toEducatorSecretJson :: PassPhrase -> KeyResources n -> KeyJson
toEducatorSecretJson pp KeyResources{..} =
    let kjEncSecretKey = encrypt pp _krSecretKey
    in KeyJson{..}

fromEducatorSecretJson :: MonadThrow m => PassPhrase -> KeyJson -> m (KeyResources n)
fromEducatorSecretJson pp KeyJson{..} = do
    sk <- decrypt pp kjEncSecretKey
        & leftToThrow SecretWrongPassPhraseError
    return $ KeyResources sk (toPublic sk)

---------------------------------------------------------------------
-- Storage operations
---------------------------------------------------------------------

-- | Where keyfile would lie.
storePath :: HasBaseConfig => KeyParams -> FilePath
storePath KeyParams{..} =
    fromMaybe defPath kpPath
  where
    defPath = bcAppDirectory baseConfig </> "educator.key"

-- | Generate store randomly.
genStore :: MonadIO m => m (KeyResources n)
genStore = do
    (_krSecretKey, _krPublicKey) <- runSecureRandom keyGen
    return KeyResources{..}

-- | Read store under given path.
readStore
    :: (MonadIO m, MonadCatch m, MonadLogging m)
    => FilePath -> PassPhrase -> m (KeyResources n)
readStore path pp = do
    logDebug $ "Reading key from: " +|| path ||+ ""
    content <- rewrapKeyIOErrors $ do
        whenPosix $ ensureModeIs mode600 path
        liftIO $ LBS.readFile path
    Versioned mid <- eitherDecode' @KeyfileContent content
        & leftToThrow (SecretParseError . toText)
    fromEducatorSecretJson pp mid

-- | Write given secret to store.
writeStoreDumb
    :: FilePath -> PassPhrase -> KeyResources n -> IO ()
writeStoreDumb path pp store =
    LBS.writeFile path $
    encode @KeyfileContent $
    Versioned $ toEducatorSecretJson pp store

-- | Write given secret to store, setting appropriate access mode.
writeStore
    :: (MonadIO m, MonadCatch m)
    => FilePath -> PassPhrase -> KeyResources n -> m ()
writeStore path pp store = liftIO . rewrapKeyIOErrors $ do
    D.createDirectoryIfMissing True (FP.takeDirectory path)
    whenPosix $ do
        LBS.writeFile path mempty
        setMode mode600 path
    writeStoreDumb path pp store

-- | Creates new store under given path.
-- If file already exists, error is thrown.
createStore
    :: (MonadIO m, MonadCatch m, MonadLogging m)
    => FilePath -> PassPhrase -> m (KeyResources n)
createStore path pp = do
    exists <- liftIO . rewrapKeyIOErrors $ doesFileExist path
    if exists
        then throwM $ SecretFileExistsError path
        else do
            logInfo $ "Creating new educator secret key store under "+||path||+""
            store <- genStore
            writeStore path pp store
            return store

-- | Syncs with store. For now store is read-only, thus it's just read.
-- Store is also created (and assumed to be absent before this function call) if
-- dedicated flag is passed.
linkStore
    :: (MonadIO m, MonadCatch m, MonadLogging m, HasBaseConfig)
    => KeyParams -> m (KeyResources n)
linkStore params@KeyParams{..} = do
    let path = storePath params
    if kpGenNew
        then createStore path kpPassphrase
        else readStore path kpPassphrase

---------------------------------------------------------------------
-- Other
---------------------------------------------------------------------

instance (HasBaseConfig, Buildable (Proxy node)) =>
         AllocResource KeyParams (KeyResources node) where
    allocResource params =
        buildComponentR (Proxy @node |+ " keys")
            (linkStore params)
            (\_ -> pass)

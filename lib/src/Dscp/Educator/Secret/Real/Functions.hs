-- | Functions to work with educator secret key storage.

module Dscp.Educator.Secret.Real.Functions
    ( linkStore
    ) where

import Data.Aeson (eitherDecode', encode)
import qualified Data.ByteString.Lazy as LBS
import Fmt ((+|), (|+))
import Loot.Log (MonadLogging, logInfo)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Dscp.Config (HasBaseConfig, baseConfig, bcAppDirectory)
import Dscp.Crypto (PassPhrase, decrypt, encrypt, genSecretKey, runSecureRandom)
import Dscp.Educator.Secret.Real.Error (EducatorSecretError (..), rewrapSecretIOErrors)
import Dscp.Educator.Secret.Real.Types (EducatorSecret (..), EducatorSecretJson (..),
                                        EducatorSecretParams (..), KeyfileContent)
import Dscp.System (ensureModeIs, mode600, setMode, whenPosix)
import Dscp.Util (leftToThrow)
import Dscp.Util.Aeson (Versioned (..))

---------------------------------------------------------------------
-- Conversions
---------------------------------------------------------------------

toEducatorSecretJson :: PassPhrase -> EducatorSecret -> EducatorSecretJson
toEducatorSecretJson pp EducatorSecret{..} =
    let esjEncSecretKey = encrypt pp esSecretKey
    in EducatorSecretJson{..}

fromEducatorSecretJson :: MonadThrow m => PassPhrase -> EducatorSecretJson -> m EducatorSecret
fromEducatorSecretJson pp EducatorSecretJson{..} = do
    esSecretKey <- decrypt pp esjEncSecretKey
        & leftToThrow SecretWrongPassPhraseError
    return EducatorSecret{..}

---------------------------------------------------------------------
-- Storage operations
---------------------------------------------------------------------

-- | Where keyfile would lie.
storePath :: HasBaseConfig => EducatorSecretParams -> FilePath
storePath EducatorSecretParams{..} =
    fromMaybe defPath espPath
  where
    defPath = bcAppDirectory baseConfig </> "educator.key"

-- | Generate store randomly.
genStore :: MonadIO m => m EducatorSecret
genStore = do
    esSecretKey <- runSecureRandom genSecretKey
    return EducatorSecret{..}

-- | Read store under given path.
readStore
    :: (MonadIO m, MonadCatch m, MonadLogging m)
    => FilePath -> PassPhrase -> m EducatorSecret
readStore path pp = do
    content <- rewrapSecretIOErrors $ do
        whenPosix $ ensureModeIs mode600 path
        liftIO $ LBS.readFile path
    Versioned mid <- eitherDecode' @KeyfileContent content
        & leftToThrow (SecretDeserialisationError . toText)
    fromEducatorSecretJson pp mid

-- | Write given secret to store.
writeStoreDumb
    :: FilePath -> PassPhrase -> EducatorSecret -> IO ()
writeStoreDumb path pp store =
    LBS.writeFile path $
    encode @KeyfileContent $
    Versioned $ toEducatorSecretJson pp store

-- | Write given secret to store, setting appropriate access mode.
writeStore
    :: (MonadIO m, MonadCatch m)
    => FilePath -> PassPhrase -> EducatorSecret -> m ()
writeStore path pp store = liftIO . rewrapSecretIOErrors $ do
    whenPosix $ do
        LBS.writeFile path mempty
        setMode mode600 path
    writeStoreDumb path pp store

-- | Creates new store under given path.
-- If file already exists, error is thrown.
createStore
    :: (MonadIO m, MonadCatch m, MonadLogging m)
    => FilePath -> PassPhrase -> m EducatorSecret
createStore path pp = do
    exists <- liftIO . rewrapSecretIOErrors $ doesFileExist path
    if exists
        then throwM $ SecretFileExistsError path
        else do
            logInfo $ "Creating new educator secret key store under "+|path|+""
            store <- genStore
            writeStore path pp store
            return store

-- | Syncs with store. For now store is read-only, thus it's just read.
-- Store is also created (and assumed to be absent before this function call) if
-- dedicated flag is passed.
linkStore
    :: (HasBaseConfig, MonadIO m, MonadCatch m, MonadLogging m)
    => EducatorSecretParams -> m EducatorSecret
linkStore params@EducatorSecretParams{..} = do
    let path = storePath params
    if espGenNew
        then createStore path espPassphrase
        else readStore path espPassphrase

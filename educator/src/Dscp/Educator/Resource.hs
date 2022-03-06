{-# LANGUAGE OverloadedLabels #-}

module Dscp.Educator.Resource
    ( BaseKeyParams
    , BaseKeyParamsRec
    , BaseKeyParamsRecP
    , defaultBaseKeyParams

    , KeyResources (..)

    , KeyJson (..)
    , KeyfileContent
    , krSecretKeyData
    , krSecretKey
    , krPublicKey

    , ourSecretKey
    , ourPublicKey
    , ourAddress
    , ourSecretKeyData

    , KeyInitError (..)
    , rewrapKeyIOErrors

    , toKeyfileContent
    , fromKeyfileContent
    , genStore
    , readStore
    , linkStore
    , toSecretJson
    ) where

import Universum

import Control.Lens (Getter, makeLenses, to)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode', encode, object, withObject, (.:),
                   (.=))
import qualified Data.ByteString.Lazy as LBS
import Fmt (Buildable (..), (+|), (+||), (|+), (||+), pretty)
import Loot.Base.HasLens (HasLens', lensOf)
import Loot.Config (Config, PartialConfig, (:::), (?~))
import Loot.Log (MonadLogging, logDebug, logInfo)
import qualified System.Directory as D
import System.FilePath ((</>))
import qualified System.FilePath as FP
import qualified Text.Show

import Dscp.Config
import Dscp.Core
import Dscp.Crypto
import Dscp.Resource.AppDir
import Dscp.System (checkFileMode, mode600, setMode, whenPosix)
import Dscp.Util (leftToThrow, wrapRethrow)
import Dscp.Util.Aeson (Base64Encoded, EncodeSerialised (..), Versioned (..))
import Dscp.Util.Test

-- | Contains all parameters required for manipulating with secret key.
type BaseKeyParams =
   '[ "path"       ::: Maybe FilePath
      -- Path to file with secret key.
      -- If not specified, some default OS-dependent path is used.
    , "genNew"     ::: Bool
      -- When 'True', file with secret key is expected to be
      -- absent and will be generated from scratch.
    , "passphrase" ::: Maybe PassPhrase
      -- When 'False', file should be present and it will be used.
      -- Password of encrypted secret key stored on disk.
    ]

type BaseKeyParamsRec = Config BaseKeyParams
type BaseKeyParamsRecP = PartialConfig BaseKeyParams

defaultBaseKeyParams :: BaseKeyParamsRecP
defaultBaseKeyParams = mempty
    & option #path       ?~ Nothing
    & option #genNew     ?~ False
    & option #passphrase ?~ Nothing

-- | Context providing access to secret key.
--
-- For now we assume secret key to be read-only.
-- Also we assume applicatiion to be autonomous, so secret key is kept
-- unencrypted during the whole operation of application.
--
-- It may be used for various purposes, in order not to mix them we carry
-- phantom type in it.
newtype KeyResources who = KeyResources
    { _krSecretKeyData :: SecretKeyData
      -- ^ Some secret key.
    }

makeLenses ''KeyResources

krSecretKey :: Getter (KeyResources who) SecretKey
krSecretKey = krSecretKeyData . to skSecret

krPublicKey :: Getter (KeyResources who) PublicKey
krPublicKey = krSecretKeyData . to skPublic

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

-- | Intermediate form of 'KeyResources' for JSON serialization.
data KeyJson = KeyJson
    { kjEncSecretKey :: EncodeSerialised Base64Encoded (Encrypted SecretKey)
    } deriving (Eq, Show)

-- | What exactly lies in the store.
type KeyfileContent = Versioned KeyJson

instance ToJSON KeyJson where
    toJSON KeyJson{..} = object
        [ "secret" .= kjEncSecretKey
        ]

instance FromJSON KeyJson where
    parseJSON = withObject "secret storage" $ \o -> do
        kjEncSecretKey <- o .: "secret"
        return KeyJson{..}

instance Arbitrary KeyJson where
    arbitrary = KeyJson <$> arbitrary

---------------------------------------------------------------------
-- HasLens
---------------------------------------------------------------------

-- | Get stored secret key. Parametrized as soon as we may keep several
-- different key resources.
ourSecretKey
    :: forall node ctx m.
       (HasLens' ctx (KeyResources node), MonadReader ctx m)
    => m SecretKey
ourSecretKey = view $ lensOf @(KeyResources node) . krSecretKeyData . to skSecret

ourPublicKey
    :: forall node ctx m.
       (HasLens' ctx (KeyResources node), MonadReader ctx m)
    => m PublicKey
ourPublicKey = view $ lensOf @(KeyResources node) . krSecretKeyData . to skPublic

ourAddress
    :: forall node ctx m.
       (HasLens' ctx (KeyResources node), MonadReader ctx m)
    => m Address
ourAddress = view $ lensOf @(KeyResources node) . krSecretKeyData . to skAddress

ourSecretKeyData
    :: forall node ctx m.
       (MonadReader ctx m, HasLens' ctx (KeyResources node))
    => m SecretKeyData
ourSecretKeyData = view $ lensOf @(KeyResources node) . krSecretKeyData

----------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------

-- | Exception during secret key extraction from storage.
data KeyInitError
    = SecretDecryptionError DecryptionError
    | SecretParseError Text
    | SecretConfMismatch Text
    | SecretIOError Text
    | SecretFileModeError Text

instance Show KeyInitError where
    show = toString @Text . pretty

instance Buildable KeyInitError where
    build = \case
        SecretDecryptionError msg ->
            "Error while decrypting educator key: "+|msg|+
            ". Either retype password or" +|
            "make sure educator.key file corresponds to the right account"
        SecretParseError _ ->
            "Invalid educator secret key storage format"
        SecretConfMismatch msg ->
            "Configuration/CLI params mismatch: "+|msg|+""
        SecretIOError msg ->
            "Some I/O error occured: "+|msg|+""
        SecretFileModeError msg ->
            "File permission error: "+|msg|+""

instance Exception KeyInitError

rewrapKeyIOErrors :: MonadCatch m => m a -> m a
rewrapKeyIOErrors = wrapRethrow @SomeException (SecretIOError . show)

---------------------------------------------------------------------
-- Conversions
---------------------------------------------------------------------

toSecretJson :: PassPhrase -> SecretKey -> KeyJson
toSecretJson pp secret =
    let kjEncSecretKey = EncodeSerialised $ encrypt pp secret
    in KeyJson{..}

fromSecretJson :: MonadThrow m => PassPhrase -> KeyJson -> m SecretKey
fromSecretJson pp KeyJson{..} = do
    decrypt pp (unEncodeSerialised kjEncSecretKey)
        & leftToThrow SecretDecryptionError

toKeyfileContent :: PassPhrase -> SecretKey -> KeyfileContent
toKeyfileContent pp sk = Versioned $ toSecretJson pp sk

fromKeyfileContent
    :: MonadThrow m
    => PassPhrase -> KeyfileContent -> m SecretKey
fromKeyfileContent pp (Versioned content) = fromSecretJson pp content

---------------------------------------------------------------------
-- Storage operations
---------------------------------------------------------------------

-- | Where keyfile would lie.
storePath
    :: Buildable (Proxy node)
    => BaseKeyParamsRec -> AppDir -> Proxy node -> FilePath
storePath baseKeyParams (AppDir appDir) nodeNameP =
    fromMaybe defPath (baseKeyParams ^. option #path)
  where
    defPath = appDir </> (nodeNameP |+ ".key")

-- | Generate key resources randomly.
genStore :: MonadIO m => m (KeyResources n)
genStore = KeyResources . secretKeyDataFromPair <$> runSecureRandom keyGen

-- | Read store under given path.
readStore
    :: (MonadIO m, MonadCatch m, MonadLogging m, MonadThrow m)
    => FilePath -> PassPhrase -> m (KeyResources n)
readStore path pp = do
    logDebug $ "Reading key from: " +|| path ||+ ""
    content <- rewrapKeyIOErrors $ do
        whenPosix $ (checkFileMode mode600 path)
            >>= leftToThrow SecretFileModeError
        liftIO $ LBS.readFile path
    Versioned mid <- eitherDecode' @KeyfileContent content
        & leftToThrow (SecretParseError . toText)
    KeyResources . mkSecretKeyData <$> fromSecretJson pp mid

-- | Write given secret to store.
writeStoreDumb
    :: FilePath -> PassPhrase -> KeyResources n -> IO ()
writeStoreDumb path pp store =
    LBS.writeFile path $
    encode @KeyfileContent $
    Versioned $ toSecretJson pp (skSecret $ _krSecretKeyData store)

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
createStore ::
       forall m n.
       ( MonadIO m
       , MonadCatch m
       , MonadLogging m
       , Buildable (Proxy n)
       )
    => FilePath
    -> PassPhrase
    -> m (KeyResources n)
createStore path pp = do
     logInfo $ "Creating new "+|nodeNameP|+" secret key under "+||path||+""
     store <- genStore
     writeStore path pp store
     return store
  where
    nodeNameP = Proxy :: Proxy n

-- | Syncs with store. For now store is read-only, thus it's just read.
-- Store is also created (and assumed to be absent before this function call) if
-- dedicated flag is passed.
linkStore
    :: forall m n.
       (MonadIO m, MonadCatch m, MonadLogging m, MonadThrow m,
        Buildable (Proxy n))
    => BaseKeyParamsRec -> AppDir -> m (KeyResources n)
linkStore baseKeyParams appDir = do
    let path = storePath baseKeyParams appDir (Proxy :: Proxy n)
        pp = fromMaybe emptyPassPhrase $ baseKeyParams ^. option #passphrase
    keyExists <- liftIO . rewrapKeyIOErrors $ D.doesFileExist path
    if (baseKeyParams ^. option #genNew) && not keyExists
        then createStore path pp
        else readStore path pp

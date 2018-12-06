-- | Functions to work with key storage.
-- Later this file will also provide possibility to choose a key from
-- the predefined test genesis instead of providing a key explicitly.

module Dscp.Resource.Keys.Functions
    ( toKeyfileContent
    , fromKeyfileContent
    , genStore
    , mkCommitteeStore
    , readStore
    , linkStore
    , toSecretJson
    ) where

import Data.Aeson (eitherDecode', encode)
import qualified Data.ByteString.Lazy as LBS
import Fmt ((+|), (+||), (|+), (||+))
import Loot.Log (MonadLogging, logDebug, logInfo)
import qualified System.Directory as D
import System.FilePath ((</>))
import qualified System.FilePath as FP

import Dscp.Core
import Dscp.Crypto
import Dscp.Resource.AppDir
import Dscp.Resource.Keys.Error (KeyInitError (..), rewrapKeyIOErrors)
import Dscp.Resource.Keys.Types (BaseKeyParams (..), CommitteeParams (..), KeyJson (..),
                                 KeyResources (..), KeyfileContent)
import Dscp.System (checkFileMode, mode600, setMode, whenPosix)
import Dscp.Util (leftToThrow)
import Dscp.Util.Aeson (EncodeSerialised (..), Versioned (..))

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
    => BaseKeyParams -> AppDir -> Proxy node -> FilePath
storePath BaseKeyParams{..} appDir nodeNameP =
    fromMaybe defPath bkpPath
  where
    defPath = appDir </> (nodeNameP |+ ".key")

-- | Generate key resources randomly.
genStore :: MonadIO m => m (KeyResources n)
genStore = KeyResources . secretKeyDataFromPair <$> runSecureRandom keyGen

-- | Generate key resources with respect to given committe parameters.
mkCommitteeStore ::
       (HasCoreConfig, MonadThrow m, MonadLogging m)
    => CommitteeParams
    -> m (KeyResources n)
mkCommitteeStore comParams = do
    _krSecretKeyData <-
        case comParams of
          CommitteeParamsOpen i -> do
              logInfo "Creating open committee key"
              sec <- case gcGovernance (giveL @CoreConfig @GenesisConfig) of
                         GovCommittee (CommitteeOpen{..}) -> do
                             when (i >= commN) $
                                 throwM $ SecretConfMismatch $
                                 "Index passed GOE than comm size: " <> show (i,commN)
                             pure commSecret
                         x -> throwM $ SecretConfMismatch $
                                       "Params were passed for open committee, but " <>
                                       "config specifies: " <> show x
              let sk = committeeDerive sec i
              pure (mkSecretKeyData sk)
          CommitteeParamsClosed {..} -> do
              logInfo "Creating closed committee key"
              let addrs = case gcGovernance (giveL @CoreConfig @GenesisConfig) of
                              GovCommittee (CommitteeClosed a) -> a
                              x -> throwM $ SecretConfMismatch $
                                            "Params were passed for closed committee, but " <>
                                            "config specifies: " <> show x

              let sk = committeeDerive cpSecret cpParticipantN
              let pk = toPublic sk
              when (mkAddr pk `notElem` addrs) $
                  throwM $ SecretConfMismatch $ "Provided secret and index doesn't " <>
                                                "belong to the list of addrs"
              pure $ secretKeyDataFromPair (sk,pk)
    return KeyResources{..}

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
        HasCoreConfig, Buildable (Proxy n))
    => BaseKeyParams -> AppDir -> m (KeyResources n)
linkStore params@BaseKeyParams{..} appDir = do
    let path = storePath params appDir (Proxy :: Proxy n)
        pp = fromMaybe emptyPassPhrase bkpPassphrase
    keyExists <- liftIO . rewrapKeyIOErrors $ D.doesFileExist path
    if bkpGenNew && not keyExists
        then createStore path pp
        else readStore path pp

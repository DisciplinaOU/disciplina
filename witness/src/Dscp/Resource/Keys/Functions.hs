-- | Functions to work with key storage.
-- Later this file will also provide possibility to choose a key from
-- the predefined test genesis instead of providing a key explicitly.

module Dscp.Resource.Keys.Functions
    ( genStore
    , linkStore
    ) where

import Data.Aeson (eitherDecode', encode)
import qualified Data.ByteString.Lazy as LBS
import Fmt ((+|), (+||), (|+), (||+))
import Loot.Log (MonadLogging, logDebug, logInfo)
import qualified System.Directory as D
import System.FilePath ((</>))
import qualified System.FilePath as FP

import Dscp.Core
import Dscp.Crypto (PassPhrase, decrypt, emptyPassPhrase, encrypt, keyGen, runSecureRandom,
                    toPublic)
import Dscp.Resource.AppDir
import Dscp.Resource.Keys.Error (KeyInitError (..), rewrapKeyIOErrors)
import Dscp.Resource.Keys.Types (BaseKeyParams (..), CommitteeParams (..), KeyJson (..),
                                 KeyResources (..), KeyfileContent)
import Dscp.System (ensureModeIs, mode600, setMode, whenPosix)
import Dscp.Util (leftToThrow)
import Dscp.Util.Aeson (CustomEncoding (..), Versioned (..))
import Dscp.Witness.Config

---------------------------------------------------------------------
-- Conversions
---------------------------------------------------------------------

toSecretJson :: PassPhrase -> KeyResources n -> KeyJson
toSecretJson pp KeyResources{..} =
    let kjEncSecretKey = CustomEncoding $ encrypt pp _krSecretKey
    in KeyJson{..}

fromSecretJson :: MonadThrow m => PassPhrase -> KeyJson -> m (KeyResources n)
fromSecretJson pp KeyJson{..} = do
    sk <- decrypt pp (unCustomEncoding kjEncSecretKey)
        & leftToThrow SecretWrongPassPhraseError
    return $ KeyResources sk (toPublic sk)

---------------------------------------------------------------------
-- Storage operations
---------------------------------------------------------------------

-- | Where keyfile would lie.
storePath
    :: (HasWitnessConfig, Buildable (Proxy node))
    => BaseKeyParams -> AppDir -> Proxy node -> FilePath
storePath BaseKeyParams{..} appDir nodeNameP =
    fromMaybe defPath bkpPath
  where
    defPath = appDir </> (nodeNameP |+ ".key")

-- | Generate key resources with respect to given committe parameters if
-- specified, otherwise randomly.
genStore ::
       (HasWitnessConfig, MonadThrow m, MonadIO m, MonadLogging m)
    => Maybe CommitteeParams
    -> m (KeyResources n)
genStore comParamsM = do
    (_krSecretKey, _krPublicKey) <-
        case comParamsM of
          Nothing -> do
              logInfo "Generating random key"
              runSecureRandom keyGen
          Just (CommitteeParamsOpen i) -> do
              logInfo "Creating open committee key"
              sec <- case gcGovernance (giveL @WitnessConfig @GenesisConfig) of
                         GovCommittee (CommitteeOpen{..}) -> do
                             when (i >= commN) $
                                 throwM $ SecretConfMismatch $
                                 "Index passed GOE than comm size: " <> show (i,commN)
                             pure commSecret
                         x -> throwM $ SecretConfMismatch $
                                       "Params were passed for open committee, but " <>
                                       "config specifies: " <> show x
              let sk = committeeDerive sec i
              pure (sk, toPublic sk)
          Just (CommitteeParamsClosed {..}) -> do
              logInfo "Creating closed committee key"
              let addrs = case gcGovernance (giveL @WitnessConfig @GenesisConfig) of
                              GovCommittee (CommitteeClosed a) -> a
                              x -> throwM $ SecretConfMismatch $
                                            "Params were passed for closed committee, but " <>
                                            "config specifies: " <> show x

              let sk = committeeDerive cpSecret cpParticipantN
              let pk = toPublic sk
              when (mkAddr pk `notElem` addrs) $
                  throwM $ SecretConfMismatch $ "Provided secret and index doesn't " <>
                                                "belong to the list of addrs"
              pure (sk,pk)
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
    fromSecretJson pp mid

-- | Write given secret to store.
writeStoreDumb
    :: FilePath -> PassPhrase -> KeyResources n -> IO ()
writeStoreDumb path pp store =
    LBS.writeFile path $
    encode @KeyfileContent $
    Versioned $ toSecretJson pp store

-- | Write given secret to store, setting appropriate access mode.
writeStore
    :: (MonadIO m, MonadCatch m, HasWitnessConfig)
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
       , HasWitnessConfig
       )
    => FilePath
    -> Maybe CommitteeParams
    -> PassPhrase
    -> m (KeyResources n)
createStore path comParamsM pp = do
     logInfo $ "Creating new "+|nodeNameP|+" secret key under "+||path||+""
     store <- genStore comParamsM
     writeStore path pp store
     return store
  where
    nodeNameP = Proxy :: Proxy n

-- | Syncs with store. For now store is read-only, thus it's just read.
-- Store is also created (and assumed to be absent before this function call) if
-- dedicated flag is passed.
linkStore
    :: forall m n.
       (MonadIO m, MonadCatch m, MonadLogging m,
        HasWitnessConfig, Buildable (Proxy n))
    => BaseKeyParams -> Maybe CommitteeParams -> AppDir -> m (KeyResources n)
linkStore params@BaseKeyParams{..} commParamsM appDir = do
    let path = storePath params appDir (Proxy :: Proxy n)
        pp = fromMaybe emptyPassPhrase bkpPassphrase
    keyExists <- liftIO . rewrapKeyIOErrors $ D.doesFileExist path
    if bkpGenNew && not keyExists
        then createStore path commParamsM pp
        else readStore path pp

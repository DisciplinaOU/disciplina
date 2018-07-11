-- | Snowdrop executor.

module Dscp.Snowdrop.Executor
    ( NodeConf(..)
    , executor
    ) where

import Control.Monad.Free (Free (..))
import qualified Data.Tree.AVL as AVL
import Snowdrop.Model.Execution (DbModifyActions)
import Snowdrop.Util (gett)

import Dscp.AVL (AvlHash)
import Dscp.Crypto (PublicKey)
import Dscp.Snowdrop.Configuration (Ids, Values)
import Dscp.Snowdrop.Serialise ()
import Dscp.Snowdrop.Storage.Avlp (ClientError (..), RootHash (..), avlClientDbActions,
                                   avlServerDbActions, deserialiseM, initAVLPureStorage)
import Dscp.Snowdrop.Storage.Pure (blockDbActions)


data NodeConf stateChgAccum blockChgAccum proof = NodeConf
    { -- TODO in real application we probably need RawBlund in storage
      -- to reply to others nodes.
      nsStateDBActions :: DbModifyActions stateChgAccum Ids Values IO proof
    , nsBlockDBActions :: DbModifyActions blockChgAccum Ids Values IO ()
    , nsPk             :: PublicKey
    }

executor :: IO ()
executor = do
    avlInitState <- initAVLPureStorage @Ids @Values initAccounts
    (serverStDba, serverLookupHash) <- avlServerDbActions avlInitState
    serverBlkDba <- blockDbActions
    let retrieveF :: AvlHash -> IO (Maybe ByteString)
        retrieveF h = serverLookupHash h >>= \resp -> do
          whenJust resp $ \resp' -> do
            (respV :: AVL.MapLayer AvlHash Ids Values AvlHash) <- deserialiseM resp'
            let respProof = AVL.Proof . Free $ pure <$> respV
            when (not $ AVL.checkProof h respProof) $ throwM BrokenProofError
          pure resp
    let (rootHash :: RootHash) = gett avlInitState
    clientBlkDba <- blockDbActions
    clientStDba <- avlClientDbActions retrieveF rootHash
    let _serverNc remForProof = NodeConf (serverStDba remForProof) serverBlkDba blockPK
    let _clientNc clientMode = NodeConf (clientStDba clientMode) clientBlkDba blockPK

    putTextLn "I'm partially compiling"
  where
    initAccounts :: Map Ids Values
    initAccounts = mempty -- TODO fill initial map with something

    blockPK = error "TBD DSCP-133"

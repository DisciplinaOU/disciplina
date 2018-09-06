-- | DB-related actions.

module Dscp.Snowdrop.Actions
    ( SDVars
    , SDActionsM (..)
    , initSDActions
    , sdActionsComposition
    ) where

import Control.Monad.Free (Free (..))
import qualified Data.Map as M
import qualified Data.Tree.AVL as AVL
import Loot.Log.Rio (LoggingIO)
import Snowdrop.Execution (DbModifyActions (..), SumChangeSet, avlClientDbActions)
import Snowdrop.Util (RIO, gett)

import Data.Time.Clock (UTCTime (..))
import qualified Snowdrop.Block as SD

import Dscp.Core
import Dscp.Crypto
import Dscp.Snowdrop.Configuration (BlockPlusAVLComposition, Ids (..), Values (..))
import Dscp.Snowdrop.Serialise ()
import Dscp.Snowdrop.Storage.Pure (blockDbActions)
import Dscp.Snowdrop.Types
import Dscp.Witness.AVL (AvlHash (..), AvlProof)
import Dscp.Witness.Config
import Snowdrop.Execution (AVLChgAccum, ClientError (..), CompositeChgAccum, DbAccessActions,
                           RememberForProof, RootHash, avlServerDbActions,
                           constructCompositeActions, deserialiseM, dmaAccessActions,
                           initAVLPureStorage)

-- It should be something more complex than IO.
type SDVars = SDActionsM (RIO LoggingIO)

-- Parameter m will be instantiated with RIO Context when the context is defined.
data SDActionsM m = SDActionsM
    { nsStateDBActions  :: RememberForProof
                        -> DbModifyActions (AVLChgAccum AvlHash Ids Values) Ids Values m AvlProof
    , nsBlockDBActions  :: DbModifyActions (SumChangeSet Ids Values) Ids Values m ()
    , nsSDParamsBuilder :: SD.OSParamsBuilder
    }

initSDActions ::
       forall m n.
       (MonadIO m, MonadCatch m, MonadIO n, MonadCatch n, HasWitnessConfig)
    => m (SDActionsM n)
initSDActions = do
    avlInitState <- liftIO $ initAVLPureStorage @Ids @Values initAccounts
    (serverStDba, serverLookupHash) <- avlServerDbActions @Ids @Values @n avlInitState
    serverBlkDba <- liftIO $ blockDbActions
    let startTime = UTCTime (toEnum 0) (toEnum 0)
    let nsSDParamsBuilder = SD.OSParamsBuilder (const $ SD.OSParams startTime startTime)
    let sdActions = SDActionsM serverStDba serverBlkDba nsSDParamsBuilder

    -- This is something to be used by AVL client (???)
    let retrieveF :: AvlHash -> m (Maybe ByteString)
        retrieveF h = serverLookupHash h >>= \resp -> do
          whenJust resp $ \resp' -> do
            (respV :: AVL.MapLayer AvlHash Ids Values AvlHash) <- deserialiseM resp'
            let respProof = AVL.Proof . Free $ pure <$> respV
            when (not $ AVL.checkProof h respProof) $ throwM BrokenProofError
          pure resp
    let (rootHash :: RootHash AvlHash) = gett avlInitState

    -- What am I supposed to do with it?
    -- pva701: it will be used in a AVL client, keep it compatible
    _clientStDba <- avlClientDbActions @Ids @Values @m retrieveF rootHash

    pure sdActions
  where

    genesisBlockAddress = mkAddr $ toPublic genesisSk
    totalCoins = totalCoinsAddrMap $ giAddressMap genesisInfo

    initAccounts :: Map Ids Values
    initAccounts = M.fromList
        [
          ( AccountInIds (AccountId genesisBlockAddress)
          , AccountOutVal (Account (coinToInteger totalCoins) 0)
          )
        ]

sdActionsComposition
  :: MonadCatch m
  => RememberForProof
  -> SDActionsM m
  -> DbAccessActions
      (CompositeChgAccum
          (SumChangeSet Ids Values)
          (AVLChgAccum AvlHash Ids Values)
          BlockPlusAVLComposition)
      Ids
      Values
      m
sdActionsComposition remForProof SDActionsM{..} =
    constructCompositeActions
        @BlockPlusAVLComposition
        (dmaAccessActions nsBlockDBActions)
        (dmaAccessActions $ nsStateDBActions remForProof)

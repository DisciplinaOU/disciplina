{-# LANGUAGE OverloadedLists #-}

-- | Node workers

module Dscp.Workers.Worker
    ( witnessWorkers
    ) where

import Control.Concurrent (threadDelay)
import Control.Lens (views)
import Data.Default (def)
import Data.Reflection (reify)
import qualified Data.Set as S
import Fmt ((+||), (||+))
import Loot.Base.HasLens (lensOf)
import Loot.Log (logError, logInfo)
import Loot.Network.Class (ClientEnv)
import Loot.Network.ZMQ (ZmqTcp)
import qualified Snowdrop.Model.Block as SD
import qualified Snowdrop.Model.Execution as SD
import qualified Snowdrop.Model.State.Core as SD
import qualified Snowdrop.Util as SD

import Dscp.AVL (AvlProof)
import Dscp.Core
import Dscp.Crypto (sign, toPublic)
import Dscp.Network.Messages (PingBlk (..), PingTx (..), PongBlk (..), PongTx (..))
import Dscp.Network.Wrapped (Worker (..), cliRecvResp, cliSend, msgType)
import Dscp.Resource.Keys (ourPublicKey, ourSecretKey)
import Dscp.Slotting (waitUntilNextSlot)
import Dscp.Snowdrop.Actions (SDActions, nsBlockDBActions, nsStateDBActions)
import Dscp.Snowdrop.Configuration (Exceptions, Ids, blockPrefix, tipPrefix)
import Dscp.Snowdrop.Expanders (expandBlock)
import Dscp.Snowdrop.Storage.Avlp (RememberForProof (..))
import Dscp.Snowdrop.Validators (blkStateConfig)
import Dscp.Witness.Launcher (WitnessWorkMode)
import Dscp.Witness.Mempool (MempoolVar, swapTxsMempool)


witnessWorkers :: WitnessWorkMode ctx m => [Worker m]
witnessWorkers = [blockIssuingWorker, witnessTxWorker, witnessBlkWorker]

----------------------------------------------------------------------------
-- Block creation
----------------------------------------------------------------------------

createPayload :: MonadIO m => MempoolVar -> m BlockBody
createPayload v = BlockBody <$> swapTxsMempool v

createBlock :: WitnessWorkMode ctx m => m Block
createBlock = do
    blockDBA <- views (lensOf @SDActions) (SD.dmaAccessActions . nsBlockDBActions)
    (tipMb, diff) <- liftIO $ SD.runERoCompIO @Exceptions blockDBA def $ do
        (tipMb :: Maybe HeaderHash) <- getTip
        let resolveTip tip = do
                blundM <- SD.queryOne (SD.BlockRef tip)
                let headerM = SD.blkHeader . SD.buBlock <$> blundM
                pure $ fromMaybe (rbHeader genesisBlock) headerM
        (diff :: Difficulty) <-
            maybe (pure $ Difficulty 1) (fmap ((+1) . hDifficulty) . resolveTip) tipMb
        pure (tipMb, diff)

    let tip = fromMaybe genesisHash tipMb

    payload <- createPayload =<< view (lensOf @MempoolVar)
    sk <- ourSecretKey
    let sgn = sign sk $ BlockToSign diff tip payload
    let header = Header sgn (toPublic sk) diff tip
    let block = Block header payload
    pure block
  where
    getTip =
        SD.unTipValue <$>
            (SD.queryOne SD.TipKey >>=
             maybe (SD.throwLocalError @(SD.BlockStateException Ids) SD.TipNotFound) pure)

applyBlock :: WitnessWorkMode ctx m => Block -> m AvlProof
applyBlock block = do
    (sdActions :: SDActions) <- view (lensOf @SDActions)
    let blockDBM = nsBlockDBActions sdActions
    let stateDBM = nsStateDBActions sdActions (RememberForProof True)

    pk <- ourPublicKey

    (blockCS, stateCS) <- reify blockPrefixes $ \(_ :: Proxy ps) ->
        let actions = SD.constructCompositeActions @ps (SD.dmaAccessActions blockDBM)
                                                       (SD.dmaAccessActions stateDBM)
            rwComp = do
              sblock <- SD.liftERoComp $ expandBlock block
              SD.applyBlock (blkStateConfig pk) sblock
         in liftIO $
            SD.runERwCompIO actions def rwComp >>=
                \((), (SD.CompositeChgAccum blockCS_ stateCS_)) -> pure (blockCS_, stateCS_)
    proof <- liftIO $ SD.dmaApply stateDBM stateCS
    liftIO $ SD.dmaApply blockDBM blockCS
    pure proof
  where
    blockPrefixes = S.fromList [tipPrefix, blockPrefix]

blockIssuingWorker :: forall ctx m. WitnessWorkMode ctx m => Worker m
blockIssuingWorker =
    Worker "blockIssuingWorker" [] [] (\btq -> action btq `catchAny` handler)
  where
    handler e = logError $ fromString $ "Exception in blockIssuingWorker: " <> show e
    action :: ClientEnv ZmqTcp -> m ()
    action _btq = forever $ do
        slotId <- waitUntilNextSlot
        logInfo $ "New slot has just started: " +|| slotId ||+ ""
        block <- createBlock
        logInfo "Created a new block"
        proof <- applyBlock block
        logInfo $ "Applied block, proof: " +|| proof ||+ ""

----------------------------------------------------------------------------
-- Pinging
----------------------------------------------------------------------------


witnessTxWorker :: forall ctx m. WitnessWorkMode ctx m => Worker m
witnessTxWorker =
    Worker "txWorker" [msgType @PongTx] [] (\btq -> action btq `catchAny` handler)
  where
    handler e = logError $ fromString $ "Exception in txWorker: " <> show e
    action :: ClientEnv ZmqTcp -> m ()
    action btq = do
      logInfo "Started witness tx worker"
      forever $ do
        cliSend btq Nothing PingTx
        (nId,PongTx txt) <- cliRecvResp btq (-1)
        logInfo $ "Heard pongtx: " +|| txt ||+ " from " +|| nId ||+ ""
        liftIO $ threadDelay 1000000

witnessBlkWorker :: forall ctx m. WitnessWorkMode ctx m => Worker m
witnessBlkWorker =
    Worker "blkWorker" [msgType @PongBlk] [] (\btq -> action btq `catchAny` handler)
  where
    handler e = logError $ fromString $ "Exception in txWorker: " <> show e
    action :: ClientEnv ZmqTcp -> m ()
    action btq = do
      liftIO $ threadDelay 500000 -- for clarity of wor
      logInfo "Started witness blk worker"
      forever $ do
        cliSend btq Nothing PingBlk
        (nId,PongBlk txt) <- cliRecvResp btq (-1)
        logInfo $ "Heard pongblk: " +|| txt ||+ " from " +|| nId ||+ ""
        liftIO $ threadDelay 1000000

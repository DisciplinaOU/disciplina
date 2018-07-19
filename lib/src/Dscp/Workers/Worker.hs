{-# LANGUAGE OverloadedLists #-}

-- | Node workers

module Dscp.Workers.Worker
    ( witnessWorkers
    ) where

import Control.Concurrent (threadDelay)
import Control.Lens (views)
import Data.Default (def)
import Fmt ((+||), (||+))
import Loot.Base.HasLens (lensOf)
import Loot.Log (logError, logInfo)
import Loot.Network.Class (ClientEnv)
import Loot.Network.ZMQ (ZmqTcp)
import qualified Snowdrop.Model.Block as SD
import qualified Snowdrop.Model.Execution as SD
import qualified Snowdrop.Model.State.Core as SD
import qualified Snowdrop.Util as SD

import Dscp.Core
import Dscp.Crypto (hash, keyGen, sign, toPublic, unsafeHash, withIntSeed)
import Dscp.Network.Messages (PingBlk (..), PingTx (..), PongBlk (..), PongTx (..))
import Dscp.Network.Wrapped (Worker (..), cliRecvResp, cliSend, msgType)
import Dscp.Resource.Keys (ourSecretKey)
import Dscp.Slotting (waitUntilNextSlot)
import Dscp.Snowdrop.Actions (SDActions, nsBlockDBActions)
import Dscp.Snowdrop.Configuration (Exceptions, Ids)
import Dscp.Witness.Launcher (WitnessWorkMode)
import Dscp.Witness.Mempool (MempoolVar, swapTxsMempool)


witnessWorkers :: WitnessWorkMode ctx m => [Worker m]
witnessWorkers = [blockIssuingWorker, witnessTxWorker, witnessBlkWorker]

----------------------------------------------------------------------------
-- Block creation
----------------------------------------------------------------------------

genesisBlock :: Block
genesisBlock = Block header payload
  where
    (sk,pk) = withIntSeed 12345 keyGen
    payload = BlockBody []
    prevHash = unsafeHash ("gromak & rechka" :: Text)
    toSign = BlockToSign 0 prevHash payload
    header = Header { hSignature = sign sk toSign
                    , hIssuer = pk
                    , hDifficulty = 0
                    , hPrevHash = prevHash
                    }

genesisHash :: HeaderHash
genesisHash = hash (rbHeader genesisBlock)

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

applyBlock :: WitnessWorkMode ctx m => Block -> m ()
applyBlock = const pass

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
        applyBlock block
        logInfo "Applied block"

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

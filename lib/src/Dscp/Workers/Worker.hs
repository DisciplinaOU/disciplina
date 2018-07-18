{-# LANGUAGE OverloadedLists #-}

-- | Node workers

module Dscp.Workers.Worker
    ( witnessWorkers
    ) where

import Control.Concurrent (threadDelay)
import Fmt ((+||), (||+))
import Loot.Log (logError, logInfo)
import Loot.Network.Class (ClientEnv)
import Loot.Network.ZMQ (ZmqTcp)
--import qualified Snowdrop.Model.Block as SD
--import qualified Snowdrop.Model.Execution as SD
--import qualified Snowdrop.Model.State.Core as SD
--import qualified Snowdrop.Util as SD

import Dscp.Core
import Dscp.Network.Messages (PingBlk (..), PingTx (..), PongBlk (..), PongTx (..))
import Dscp.Network.Wrapped (Worker (..), cliRecvResp, cliSend, msgType)
import Dscp.Slotting
import Dscp.Witness.Launcher (WitnessWorkMode)
import Dscp.Witness.Mempool


witnessWorkers :: WitnessWorkMode ctx m => [Worker m]
witnessWorkers = [blockIssuingWorker, witnessTxWorker, witnessBlkWorker]

----------------------------------------------------------------------------
-- Block creation
----------------------------------------------------------------------------

_createPayload :: MonadIO m => MempoolVar -> m BlockBody
_createPayload v = BlockBody <$> swapTxsMempool v

_createBlock :: WitnessWorkMode ctx m => m Block
_createBlock = error "TBD DSCP-133"
 -- do
 --   blockDBA <- views (lensOf @SDActions) (SD.dmaAccessActions . nsBlockDBActions)
 --   (tipMb, diffMb) <- liftIO $ SD.runERoCompIO @Exceptions blockDBA mempty $ do
 --       (tipMb :: Maybe HeaderHash) <- getTip
 --       let getDiff (blund :: SBlund) =
 --               1 + hDifficulty (SD.blkHeader $ SD.buBlock blund)
 --       (,) tipMb <$> maybe (pure $ Just 1) (fmap (fmap getDiff) . SD.queryOne . SD.BlockRef) tipMb
 --   undefined
--    case diffMb of
--        Just diff -> do
--            payload <- createPayload scMempool
--            let sgn = signature $ sign scPk $ DataToSign (diff, tipMb, payload)
--            let block = RawBlock (Header sgn (toPublicKey scPk) diff tipMb) payload
--            pure block
--        Nothing   -> maybe (error "tip should be Just") (\tip -> error $ "Block for tip not found: " <> show tip) tipMb
--  where
--    getTip =
--        SD.unTipValue <$>
--        (SD.queryOne SD.TipKey >>=
--         maybe (SD.throwLocalError @(SD.BlockStateException Ids) SD.TipNotFound) pure)


blockIssuingWorker :: forall ctx m. WitnessWorkMode ctx m => Worker m
blockIssuingWorker =
    Worker "blockIssuingWorker" [] [] (\btq -> action btq `catchAny` handler)
  where
    handler e = logError $ fromString $ "Exception in blockIssuingWorker: " <> show e
    action :: ClientEnv ZmqTcp -> m ()
    action _btq = forever $ do
        slotId <- waitUntilNextSlot
        putTextLn $ "New slot has just started: " +|| slotId ||+ ""

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

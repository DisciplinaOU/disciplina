-- | Block processing logic.

module Dscp.Witness.Block.Logic
    ( createBlock
    , applyBlock
    ) where

import Control.Lens (views)
import Data.Default (def)
import Data.Reflection (reify)
import qualified Data.Set as S
import Loot.Base.HasLens (lensOf)
import qualified Snowdrop.Model.Block as SD
import qualified Snowdrop.Model.Execution as SD
import qualified Snowdrop.Model.State.Core as SD
import qualified Snowdrop.Util as SD

import Dscp.Core
import Dscp.Crypto (sign, toPublic)
import Dscp.Resource.Keys (ourPublicKey, ourSecretKey)
import Dscp.Snowdrop.Actions (SDActions, nsBlockDBActions, nsStateDBActions)
import Dscp.Snowdrop.Configuration (Exceptions, Ids, blockPrefix, tipPrefix)
import Dscp.Snowdrop.Expanders (expandBlock)
import Dscp.Snowdrop.Storage.Avlp (RememberForProof (..))
import Dscp.Snowdrop.Validators (blkStateConfig)
import Dscp.Witness.AVL (AvlProof)
import Dscp.Witness.Launcher (WitnessNode, WitnessWorkMode)
import Dscp.Witness.Mempool (MempoolVar, takeTxsMempool)

-- | Empty mempool(s), create block body.
createPayload :: MonadIO m => MempoolVar -> m BlockBody
createPayload v = BlockBody <$> takeTxsMempool v

-- | Create a public block.
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
    sk <- ourSecretKey @WitnessNode
    let sgn = sign sk $ BlockToSign diff tip payload
    let header = Header sgn (toPublic sk) diff tip
    let block = Block header payload
    pure block
  where
    getTip =
        SD.unTipValue <$>
            (SD.queryOne SD.TipKey >>=
             maybe (SD.throwLocalError @(SD.BlockStateException Ids) SD.TipNotFound) pure)

-- | Apply verified block.
applyBlock :: WitnessWorkMode ctx m => Block -> m AvlProof
applyBlock block = do
    (sdActions :: SDActions) <- view (lensOf @SDActions)
    let blockDBM = nsBlockDBActions sdActions
    let stateDBM = nsStateDBActions sdActions (RememberForProof True)

    pk <- ourPublicKey @WitnessNode

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

-- | Creating and application.

module Dscp.Witness.Logic.Processing where


import Data.Default (def)
import Data.Reflection (reify)
import qualified Data.Set as S
import Loot.Base.HasLens (lensOf)
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Model.Block as SD
import qualified Snowdrop.Model.Execution as SD

import Dscp.Core
import Dscp.Crypto
import Dscp.Resource.Keys (ourSecretKey)
import Dscp.Snowdrop
import Dscp.Witness.AVL (AvlProof)
import Dscp.Witness.Launcher.Marker
import Dscp.Witness.Launcher.Mode
import Dscp.Witness.Logic.Getters
import Dscp.Witness.Mempool (MempoolVar, takeTxsMempool)
import qualified Dscp.Witness.SDLock as Lock


-- | Empty mempool(s), create block body.
createPayload :: MonadIO m => MempoolVar -> m BlockBody
createPayload v = BlockBody <$> takeTxsMempool v

-- | Create a public block.
createBlock :: WitnessWorkMode ctx m => SlotId -> m Block
createBlock newSlot = do
    tipHeader <- runSdMRead getTipHeader
    let tipHash = headerHash tipHeader
    let diff = hDifficulty tipHeader + 1

    payload <- createPayload =<< view (lensOf @MempoolVar)
    sk <- ourSecretKey @WitnessNode
    let sgn = sign sk $ BlockToSign diff newSlot tipHash payload
    let header = Header sgn (toPublic sk) diff newSlot tipHash
    let block = Block header payload
    pure block

-- | Apply verified block.
applyBlock :: WitnessWorkMode ctx m => Block -> m AvlProof
applyBlock block = do
    Lock.writingSDLock $ do
        (sdActions :: SDActions) <- view (lensOf @SDActions)
        let blockDBM = nsBlockDBActions sdActions
        let stateDBM = nsStateDBActions sdActions (RememberForProof True)

        (blockCS, stateCS) <- reify blockPrefixes $ \(_ :: Proxy ps) ->
            let actions = SD.constructCompositeActions @ps (SD.dmaAccessActions blockDBM)
                                                           (SD.dmaAccessActions stateDBM)
                rwComp = do
                  sblock <- SD.liftERoComp $ expandBlock block
                  SD.applyBlock blkStateConfig sblock
             in SD.runERwCompIO actions def rwComp >>=
                    \((), (SD.CompositeChgAccum blockCS_ stateCS_)) -> pure (blockCS_, stateCS_)
        proof <- runSdRIO $ SD.dmaApply stateDBM stateCS
        runSdRIO $ SD.dmaApply blockDBM blockCS
        pure proof
  where
    blockPrefixes = S.fromList [tipPrefix, blockPrefix]

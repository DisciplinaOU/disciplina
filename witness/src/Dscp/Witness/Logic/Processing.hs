-- | Creating and application.

module Dscp.Witness.Logic.Processing
    ( createPayload
    , createBlock
    , applyBlock
    , applyGenesisBlock
    ) where

import Data.Default (def)
import qualified Data.Map as M
import Data.Reflection (reify)
import Loot.Base.HasLens (lensOf)
import Serokell.Util (enumerate)
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Model.Block as SD
import qualified Snowdrop.Model.Execution as SD
import qualified Snowdrop.Util as SD

import Dscp.Core
import Dscp.Crypto
import Dscp.Resource.Keys (ourSecretKey)
import Dscp.Snowdrop
import Dscp.Witness.AVL (AvlProof)
import Dscp.Witness.Launcher.Marker
import Dscp.Witness.Launcher.Mode
import Dscp.Witness.Logic.Getters
import Dscp.Witness.Mempool (takeTxsMempool)
import Dscp.Witness.SDLock


-- | Empty mempool(s), create block body.
createPayload
    :: forall ctx m
    .  (WitnessWorkMode ctx m, WithinWriteSDLock)
    => m BlockBody
createPayload = BlockBody <$> takeTxsMempool @ctx

-- | Create a public block.
createBlock
    :: (WitnessWorkMode ctx m, WithinWriteSDLock)
    => SlotId -> m Block
createBlock newSlot = do
    tipHeader <- runSdM getTipHeader
    let tipHash = headerHash tipHeader
    let diff = hDifficulty tipHeader + 1

    payload <- createPayload
    sk <- ourSecretKey @WitnessNode
    let sgn = sign sk $ BlockToSign diff newSlot tipHash payload
    let header = Header sgn (toPublic sk) diff newSlot tipHash
    let block = Block header payload
    pure block

-- | Apply verified block.
applyBlockRaw :: (WitnessWorkMode ctx m, WithinWriteSDLock) => Bool -> Block -> m AvlProof
applyBlockRaw toVerify block = do
    (sdActions :: SDActions) <- view (lensOf @SDActions)
    let blockDBM = nsBlockDBActions sdActions
    let stateDBM = nsStateDBActions sdActions (RememberForProof True)

    (blockCS, stateCS) <- reify blockPrefixes $ \(_ :: Proxy ps) ->
        let actions = SD.constructCompositeActions @ps (SD.dmaAccessActions blockDBM)
                                                       (SD.dmaAccessActions stateDBM)
            rwComp = do
              sblock <- SD.liftERoComp $ expandBlock block
              SD.applyBlockImpl toVerify blkStateConfig sblock
              sequence_ . fmap addTx . enumerate . bbTxs . bBody $ block
            addTx (idx, gTx) = SD.modifyRwCompChgAccum $ SD.ChangeSet $
                M.singleton
                    (SD.inj . toGTxId . unGTxWitnessed $ gTx)
                    (SD.New . TxVal $ TxBlockRef (headerHash block) idx)
          in SD.runERwCompIO actions def rwComp >>=
                \((), (SD.CompositeChgAccum blockCS_ stateCS_)) -> pure (blockCS_, stateCS_)
    proof <- runSdRIO $ SD.dmaApply stateDBM stateCS
    runSdRIO $ SD.dmaApply blockDBM blockCS
    pure proof

applyBlock :: (WitnessWorkMode ctx m, WithinWriteSDLock) => Block -> m AvlProof
applyBlock = applyBlockRaw True

applyGenesisBlock :: (WitnessWorkMode ctx m, WithinWriteSDLock) => m ()
applyGenesisBlock = void $ applyBlockRaw False genesisBlock

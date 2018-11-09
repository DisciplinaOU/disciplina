-- | Block application.

module Dscp.Witness.Logic.Apply
    ( applyBlockRaw
    , applyBlock
    , applyGenesisBlock
    ) where

import Data.Default (def)
import qualified Data.Map as M
import Data.Time.Clock (UTCTime (..))

import Loot.Base.HasLens (lensOf)
import Serokell.Util (enumerate)
import qualified Snowdrop.Block as SD
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Execution as SD
import qualified Snowdrop.Util as SD

import Dscp.Core
import Dscp.DB.CanProvideDB (providePlugin)
import Dscp.Snowdrop
import qualified Dscp.Snowdrop.Storage.Avlp as Avlp
import Dscp.Witness.AVL (AvlHash, AvlProof)
import Dscp.Witness.Launcher.Context
import Dscp.Witness.SDLock

-- | Apply verified block.
applyBlockRaw
    :: forall ctx m
    .  ( WitnessWorkMode ctx m
       , WithinWriteSDLock
       )
    => Bool
    -> Bool
    -> Block
    -> m AvlProof
applyBlockRaw applyFees toVerify block = do
    plugin <- providePlugin
    sdActions <- view (lensOf @SDVars)

    let sdOSParamsBuilder = nsSDParamsBuilder sdActions
    let blockDBM = nsBlockDBActions sdActions
    let stateDBM = nsStateDBActions sdActions

    (blockCS, stateCS) <-
        let actions = SD.constructCompositeActions @BlockPlusAVLComposition
                                                   (SD.dmaAccessActions blockDBM)
                                                   (SD.dmaAccessActions stateDBM)
            rwComp = do
              sblock <- SD.liftERoComp $ expandBlock applyFees block
              -- getCurrentTime requires MonadIO
              let osParams = SD.unOSParamsBuilder sdOSParamsBuilder $ UTCTime (toEnum 0) (toEnum 0)
              SD.applyBlockImpl toVerify osParams blkStateConfig (bBody block) sblock
              -- TODO: move the changeset expanding below to Dscp.Snowdrop.Expanders.expandBlock
              void $ SD.modifyRwCompChgAccum $ SD.CAMChange $ SD.ChangeSet $
                  M.singleton
                      (SD.inj . hDifficulty . bHeader $ block)
                      (SD.New . BlockIdxVal $ headerHash block)
              void $ SD.modifyRwCompChgAccum $ SD.CAMChange $ SD.ChangeSet $
                  M.singleton
                      (SD.inj . NextBlockOf . hPrevHash . bHeader $ block)
                      (SD.New . NextBlockOfVal . NextBlock $ headerHash block)
              sequence_ . fmap addTx . enumerate . bbTxs . bBody $ block

            addTx (idx, gTx) = SD.modifyRwCompChgAccum $ SD.CAMChange $ SD.ChangeSet $
                M.fromList $
                    [ ( SD.inj . TxBlockRefId . toGTxId . unGTxWitnessed $ gTx
                      , SD.New . TxBlockVal $ TxBlockRef (headerHash block) idx
                      )
                    ]
          in do
              Avlp.initAVLStorage @AvlHash plugin initAccounts

              res <- SD.runERwCompIO actions def rwComp <&>
                  \((), (SD.CompositeChgAccum blockCS_ stateCS_)) -> (blockCS_, stateCS_)

              return res

    proof <- runSdRIO $ SD.dmaApply stateDBM stateCS
    runSdRIO $ SD.dmaApply blockDBM blockCS
    pure proof

-- | Apply block with verification.
applyBlock :: (WitnessWorkMode ctx m, WithinWriteSDLock) => Block -> m AvlProof
applyBlock = applyBlockRaw True True

applyGenesisBlock :: (WitnessWorkMode ctx m, WithinWriteSDLock) => m ()
applyGenesisBlock = void $ applyBlockRaw False False genesisBlock

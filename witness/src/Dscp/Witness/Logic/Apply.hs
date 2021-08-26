-- | Block application.

module Dscp.Witness.Logic.Apply
    ( applyBlockRaw
    , applyBlock
    , applyGenesisBlock
    ) where

import Data.Default (def)
import Data.Time.Clock (UTCTime (..))

import Loot.Base.HasLens (lensOf)
import qualified Snowdrop.Block as SD
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Execution as SD

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
    -> Block
    -> m AvlProof
applyBlockRaw applyFees block = do
    plugin <- providePlugin
    sdActions <- view (lensOf @SDVars)

    let sdOSParamsBuilder = nsSDParamsBuilder sdActions
    let blockDBM = nsBlockDBActions sdActions
    let stateDBM = nsStateDBActions sdActions

    (stateCS, blockCS) <-
        let actions = sdActionsComposition sdActions
            rwComp = do
              sblock <- SD.liftERoComp $ expandBlock applyFees block
              -- getCurrentTime requires MonadIO
              let osParams = SD.unOSParamsBuilder sdOSParamsBuilder $ UTCTime (toEnum 0) (toEnum 0)
              SD.applyBlockImpl True osParams blkStateConfig (bBody block) sblock
          in do
              Avlp.initAVLStorage @AvlHash plugin initAccounts

              res <- unwrapSDBaseRethrow $
                     SD.runERwCompIO actions def rwComp <&>
                  \((), (SD.CompositeChgAccum stateCS_ blockCS_)) -> (stateCS_, blockCS_)

              return res

    proof <- runSdRIO $ SD.dmaApply stateDBM stateCS
    runSdRIO $ SD.dmaApply blockDBM blockCS
    pure proof

-- | Apply block with verification.
applyBlock :: (WitnessWorkMode ctx m, WithinWriteSDLock) => Block -> m AvlProof
applyBlock = applyBlockRaw True

applyGenesisBlock :: (WitnessWorkMode ctx m, WithinWriteSDLock) => m ()
applyGenesisBlock = void $ applyBlockRaw False genesisBlock

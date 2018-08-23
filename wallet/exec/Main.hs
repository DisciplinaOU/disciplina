module Main where

import Control.Monad.Component (ComponentM, runComponentM)
import IiExtras
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Knit.Backend
import Ariadne.TaskManager.Backend
import Ariadne.UI.Vty
import Ariadne.UI.Vty.Face
import Dscp.Wallet.Backend
import Dscp.Wallet.CLI

import qualified Ariadne.TaskManager.Knit as Knit
import qualified Ariadne.UI.Vty.Knit as Knit
import qualified Dscp.Wallet.Knit as Knit
import qualified Knit

import Glue

type UiComponents = '[Knit.Core, Knit.Wallet, Knit.TaskManager, Knit.UI]

main :: IO ()
main = do
    serverAddress <- getWalletCLIParams
    runComponentM "ariadne" (initializeEverything serverAddress) id

initializeEverything :: BaseUrl -> ComponentM (IO ())
initializeEverything serverAddress = do
    uiWalletState <- createWalletState

    let features = UiFeatures
            { featureStatus = False
            , featureExport = True
            , featureAccounts = False
            , featureTxHistory = True
            , featureFullRestore = False
            , featureSecretKeyName = "Secret key"
            }
    (uiFace, mkUiAction) <- createAriadneUI features historyToUI
    taskManagerFace <- createTaskManagerFace
    walletFace <- createWalletFace serverAddress (putWalletEventToUI uiWalletState uiFace)

    let knitExecContext :: (Doc -> IO ()) -> Knit.ExecContext IO UiComponents
        knitExecContext putCommandOutput =
            Knit.CoreExecCtx (putCommandOutput . Knit.ppValue) :&
            Knit.WalletExecCtx walletFace :&
            Knit.TaskManagerExecCtx taskManagerFace :&
            Knit.UiExecCtx uiFace :&
            RNil

        knitFace = createKnitBackend knitExecContext taskManagerFace

        uiAction :: IO ()
        uiAction = mkUiAction (knitFaceToUI uiWalletState uiFace walletFace knitFace)

    return uiAction
module Main where

import Control.Monad.Component (ComponentM, runComponentM)
import IiExtras
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Knit.Backend
import Ariadne.TaskManager.Backend
import Ariadne.UI.Cli
import Dscp.Wallet.Backend
import Dscp.Wallet.CLI

import qualified Ariadne.TaskManager.Knit as Knit
import qualified Dscp.Wallet.Knit as Knit
import qualified Knit

import Glue

type UiComponents = '[Knit.Core, Knit.TaskManager, Knit.Wallet]

main :: IO ()
main = do
    serverAddress <- getWalletCLIParams
    runComponentM "ariadne" (initializeEverything serverAddress) id

initializeEverything :: BaseUrl -> ComponentM (IO ())
initializeEverything serverAddress = do
    taskManagerFace <- createTaskManagerFace
    walletFace <- createWalletFace serverAddress (void . return)
    (uiFace, mkUiAction) <- createAriadneUI

    let knitExecContext :: (Doc -> IO ()) -> Knit.ExecContext IO UiComponents
        knitExecContext putCommandOutput =
            Knit.CoreExecCtx (putCommandOutput . Knit.ppValue) :&
            Knit.TaskManagerExecCtx taskManagerFace :&
            Knit.WalletExecCtx walletFace :&
            RNil

        knitFace = createKnitBackend knitExecContext taskManagerFace

        uiAction :: IO ()
        uiAction = mkUiAction (knitFaceToUI uiFace knitFace)

    return uiAction

module Main where

import IiExtras
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Knit.Backend
import Ariadne.TaskManager.Backend
import Ariadne.UI.Cli
import Dscp.Wallet.Backend

import qualified Ariadne.TaskManager.Knit as Knit
import qualified Dscp.Wallet.Knit as Knit
import qualified Knit

import Glue

type Components = '[Knit.Core, Knit.TaskManager, Knit.Wallet]

main :: IO ()
main = do
  taskManagerFace <- createTaskManagerFace
  walletFace <- createWalletFace
  (uiFace, mkUiAction) <- createUI

  let
    knitExecContext :: (Doc -> IO ()) -> Knit.ExecContext IO Components
    knitExecContext putCommandOutput =
      Knit.CoreExecCtx (putCommandOutput . Knit.ppValue) :&
      Knit.TaskManagerExecCtx taskManagerFace :&
      Knit.WalletExecCtx walletFace :&
      RNil

    knitFace = createKnitBackend knitExecContext taskManagerFace

    uiAction :: IO ()
    uiAction = mkUiAction (knitFaceToUI uiFace knitFace)

  uiAction

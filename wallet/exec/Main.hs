module Main where

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

type Components = '[Knit.Core, Knit.Wallet, Knit.TaskManager, Knit.UI]

main :: IO ()
main = do
  serverAddress <- getWalletCLIParams

  uiWalletState <- createWalletState

  let
    features = UiFeatures
      { featureStatus = False
      , featureExport = True
      , featureAccounts = False
      , featureFullRestore = False
      , featureSecretKeyName = "Secret key"
      }
  (uiFace, mkUiAction) <- createAriadneUI features historyToUI
  taskManagerFace <- createTaskManagerFace
  walletFace <- createWalletFace serverAddress (putWalletEventToUI uiWalletState uiFace)

  let
    knitExecContext :: (Doc -> IO ()) -> Knit.ExecContext IO Components
    knitExecContext putCommandOutput =
      Knit.CoreExecCtx (putCommandOutput . Knit.ppValue) :&
      Knit.WalletExecCtx walletFace :&
      Knit.TaskManagerExecCtx taskManagerFace :&
      Knit.UiExecCtx uiFace :&
      RNil

    knitFace = createKnitBackend knitExecContext taskManagerFace

    uiAction :: IO ()
    uiAction = mkUiAction (knitFaceToUI uiWalletState uiFace walletFace knitFace)

  uiAction

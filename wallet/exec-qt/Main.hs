module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Monad.Component (ComponentM, runComponentM)
import NType (N (..))
import System.Random (randomRIO)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Knit.Backend
import Ariadne.TaskManager.Backend
import Ariadne.UI.Qt
import Ariadne.UI.Qt.Face
import Dscp.Config
import Dscp.Core
import Dscp.Wallet.Backend
import Dscp.Wallet.CLI

import qualified Ariadne.TaskManager.Knit as Knit
import qualified Dscp.Wallet.Knit as Knit
import qualified Knit

import Glue

type UiComponents = '[Knit.Core, Knit.Wallet, Knit.TaskManager]

main :: IO ()
main = do
    params <- getWalletCLIParams
    config <- buildConfig (wpConfigParams params) fillCoreConfig
    withCoreConfig config $
        runComponentM "ariadne" (initializeEverything params) id


initializeEverything :: HasCoreConfig => WalletCLIParams -> ComponentM (IO ())
initializeEverything WalletCLIParams{..} = do
    uiWalletState <- createWalletState

--     let features = UiFeatures
--             { featureStatus = False
--             , featureExport = True
--             , featureAccounts = False
--             , featureTxHistory = True
--             , featureFullRestore = False
--             , featureSecretKeyName = "Secret key"
--             }
    let uiWalletFace = UiWalletFace
            { uiGenerateMnemonic = error "wat"
            , uiDefaultEntropySize = error "wat"
            }
    (uiFace, mkUiAction) <- createAriadneUI uiWalletFace historyToUI
    taskManagerFace <- createTaskManagerFace
    walletFace <- createWalletFace wpWitness (putWalletEventToUI uiWalletState uiFace)

    let knitExecContext :: (Doc -> IO ()) -> Knit.ExecContext IO UiComponents
        knitExecContext putCommandOutput =
            Knit.CoreExecCtx (putCommandOutput . Knit.ppValue) &:
            Knit.WalletExecCtx walletFace &:
            Knit.TaskManagerExecCtx taskManagerFace &:
            Base ()
          where
            a &: b = Step (a, b)
            infixr &:

        knitFace = createKnitBackend knitExecContext taskManagerFace

        uiAction :: IO ()
        uiAction = mkUiAction (knitFaceToUI uiWalletState uiFace walletFace knitFace)

        refreshAction :: IO ()
        refreshAction = forever $ do
            delay <- randomRIO (15, 25)
            threadDelay $ delay * 10 ^ (6 :: Int)
            walletRefreshState walletFace

    return $ uiAction `race_` refreshAction

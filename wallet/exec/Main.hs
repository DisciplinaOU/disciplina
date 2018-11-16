module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Monad.Component (ComponentM, runComponentM)
import NType (N (..))
import System.Random (randomRIO)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Dscp.Resource.Logging (allocLogging)
--import Loot.Log (Logging (..), Name, NameSelector (CallstackName, GivenName),
--                 logInfo, modifyLogName, Level(Debug))

import Ariadne.Knit.Backend
import Ariadne.TaskManager.Backend
import Ariadne.UI.Vty
import Ariadne.UI.Vty.Face
import Dscp.Wallet.Backend
import Dscp.Wallet.CLI
import Dscp.Wallet.Config

import qualified Ariadne.TaskManager.Knit as Knit
import qualified Ariadne.UI.Vty.Knit as Knit
import qualified Dscp.Wallet.Knit as Knit
import qualified Knit

import Glue

type UiComponents = '[Knit.Core, Knit.Wallet, Knit.TaskManager, Knit.UI]

main :: IO ()
main = do
    wConfig <- getWalletConfig
    withWalletConfig wConfig $
        runComponentM "ariadne" initializeEverything id

initializeEverything :: HasWalletConfig => ComponentM (IO ())
initializeEverything = do
    let logParams = giveL @WalletConfig @LoggingParams
    _ <- allocLogging logParams

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
    walletFace <- createWalletFace witnessUrl (putWalletEventToUI uiWalletState uiFace)

    let knitExecContext :: (Doc -> IO ()) -> Knit.ExecContext IO UiComponents
        knitExecContext putCommandOutput =
            Knit.CoreExecCtx (putCommandOutput . Knit.ppValue) &:
            Knit.WalletExecCtx walletFace &:
            Knit.TaskManagerExecCtx taskManagerFace &:
            Knit.UiExecCtx uiFace &:
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

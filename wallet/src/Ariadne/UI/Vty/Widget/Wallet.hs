module Ariadne.UI.Vty.Widget.Wallet
       ( initWalletWidget
       ) where

import Control.Lens (assign, ix, makeLensesWith, (%=), (.=))
import IiExtras

import qualified Brick as B
import qualified Data.Text as T

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Form.Button
import Ariadne.UI.Vty.Widget.Form.Edit
import Ariadne.UI.Vty.Widget.Form.List
import Ariadne.UI.Vty.Widget.Form.Send

----------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------

data WalletAccount =
  WalletAccount
    { walletAccountIdx :: !Word32
    , walletAccountName :: !Text
    , walletAccountBalance :: !Text
    , walletAccountSelected :: Bool
    }

data WalletWidgetState =
  WalletWidgetState
    { walletLangFace :: !UiLangFace

    , walletName :: !Text
    , walletRenameResult :: !RenameResult
    , walletBalance :: !BalanceResult

    , walletAccounts :: ![WalletAccount]
    , walletNewAccountName :: !Text
    , walletNewAccountResult :: !NewAccountResult
    }

data RenameResult
  = RenameResultNone
  | RenameResultWaiting !UiCommandId
  | RenameResultError !Text
  | RenameResultSuccess

data BalanceResult
  = BalanceResultNone
  | BalanceResultWaiting !UiCommandId
  | BalanceResultError !Text
  | BalanceResultSuccess !Text  -- ^ Balance

data NewAccountResult
  = NewAccountResultNone
  | NewAccountResultWaiting !UiCommandId
  | NewAccountResultError !Text
  | NewAccountResultSuccess

makeLensesWith postfixLFields ''WalletAccount
makeLensesWith postfixLFields ''WalletWidgetState

initWalletWidget :: UiLangFace -> Widget p
initWalletWidget langFace =
  initWidget $ do
    setWidgetDrawWithFocus drawWalletWidget
    setWidgetScrollable
    setWidgetHandleEvent handleWalletWidgetEvent
    setWidgetState WalletWidgetState
      { walletLangFace = langFace

      , walletName = ""
      , walletRenameResult = RenameResultNone
      , walletBalance = BalanceResultNone

      , walletAccounts = []
      , walletNewAccountName = ""
      , walletNewAccountResult = NewAccountResultNone
      }

    addWidgetChild WidgetNameWalletName $
      initEditWidget $ widgetParentLens walletNameL
    addWidgetChild WidgetNameWalletRenameButton $
      initButtonWidget "Rename"
    addWidgetEventHandler WidgetNameWalletRenameButton $ \case
      WidgetEventButtonPressed -> performRename
      _ -> return ()

    addWidgetChild WidgetNameWalletAccountList $
      initListWidget (widgetParentGetter walletAccounts) drawAccountRow
    addWidgetEventHandler WidgetNameWalletAccountList $ \case
      WidgetEventListSelected idx -> toggleAccount idx
      _ -> return ()

    addWidgetChild WidgetNameWalletNewAccountName $
      initEditWidget $ widgetParentLens walletNewAccountNameL
    addWidgetChild WidgetNameWalletNewAccountButton $
      initButtonWidget "Create"
    addWidgetEventHandler WidgetNameWalletNewAccountButton $ \case
      WidgetEventButtonPressed -> performNewAccount
      _ -> return ()

    addWidgetChild WidgetNameWalletSend $
      initSendWidget langFace $
        Just $ widgetParentGetter
        (\WalletWidgetState{..} -> map walletAccountIdx $ filter walletAccountSelected $ walletAccounts)

    withWidgetState updateFocusList

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

drawAccountRow :: Bool -> WalletAccount -> B.Widget WidgetName
drawAccountRow focused WalletAccount{..} =
  (if focused then B.withAttr "selected" else id) $
  B.txt $
  (if walletAccountSelected then "[X] " else "[ ] ") <> walletAccountName

drawWalletWidget :: WidgetName -> WalletWidgetState -> WidgetDrawM WalletWidgetState p (B.Widget WidgetName)
drawWalletWidget focus WalletWidgetState{..} = do
  widget <- ask
  widgetName <- getWidgetName

  let
    padBottom = B.padBottom (B.Pad 1)
    fillLeft w = T.takeEnd w . (T.append $ T.replicate w " ")

    labelWidth = 16

    drawChild = drawWidgetChild focus widget
    label = B.padRight (B.Pad 1) . B.txt . fillLeft labelWidth

  return $
    B.viewport widgetName B.Vertical $
    B.padAll 1 $
    B.vBox $
    padBottom <$>
      [ label "Wallet name:"
          B.<+> drawChild WidgetNameWalletName
      , label "Balance:" B.<+> case walletBalance of
          BalanceResultNone -> B.emptyWidget
          BalanceResultWaiting _ -> B.txt "requesting..."
          BalanceResultError err -> B.txt err
          BalanceResultSuccess balance -> B.txt balance
      , drawChild WidgetNameWalletSend
      ]

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

handleWalletWidgetEvent
  :: UiEvent
  -> WidgetEventM WalletWidgetState p ()
handleWalletWidgetEvent = \case
  UiWalletEvent UiWalletUpdate{..} -> do
    whenJust wuSelectionInfo $ \case
      UiSelectionWallet UiWalletInfo{..} -> do
        UiLangFace{..} <- use walletLangFaceL
        walletNameL .= fromMaybe "" uwiLabel
        walletAccountsL .= map
          (\(idx, UiAccountInfo{..}) -> WalletAccount idx (fromMaybe "" uaciLabel) uaciBalance False)
          (zip [0..] uwiAccounts)
        use walletBalanceL >>= \case
          BalanceResultWaiting commandId
            | Just taskId <- cmdTaskId commandId ->
                void . liftIO . langPutUiCommand $ UiKill taskId
          _ -> return ()
        liftIO (langPutUiCommand UiBalance) >>=
          assign walletBalanceL . either BalanceResultError BalanceResultWaiting
        updateFocusList
      _ -> return ()
  UiCommandResult commandId (UiRenameCommandResult result) -> do
    walletRenameResultL %= \case
      RenameResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiRenameCommandSuccess -> RenameResultSuccess
          UiRenameCommandFailure err -> RenameResultError err
      other -> other
  UiCommandResult commandId (UiBalanceCommandResult result) -> do
    walletBalanceL %= \case
      BalanceResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiBalanceCommandSuccess balance -> BalanceResultSuccess balance
          UiBalanceCommandFailure err -> BalanceResultError err
      other -> other
  UiCommandResult commandId (UiNewAccountCommandResult result) -> do
    use walletNewAccountResultL >>= \case
      NewAccountResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiNewAccountCommandSuccess -> do
            walletNewAccountResultL .= NewAccountResultSuccess
            walletNewAccountNameL .= ""
          UiNewAccountCommandFailure err -> do
            walletNewAccountResultL .= NewAccountResultError err
      _ ->
        return ()
  _ ->
    return ()

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

updateFocusList :: Monad m => StateT WalletWidgetState (StateT (WidgetInfo WalletWidgetState p) m) ()
updateFocusList = do
  accounts <- use walletAccountsL
  lift $ setWidgetFocusList $
    [ WidgetNameWalletName
    , WidgetNameWalletRenameButton
    ] ++
    (if null accounts then [] else [WidgetNameWalletAccountList]) ++
    [ WidgetNameWalletNewAccountName
    , WidgetNameWalletNewAccountButton
    , WidgetNameWalletSend
    ]

performRename :: WidgetEventM WalletWidgetState p ()
performRename = do
  UiLangFace{..} <- use walletLangFaceL
  name <- use walletNameL
  use walletRenameResultL >>= \case
    RenameResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiRename $ UiRenameArgs name) >>=
      assign walletRenameResultL . either RenameResultError RenameResultWaiting

toggleAccount :: Int -> WidgetEventM WalletWidgetState p ()
toggleAccount idx = do
  walletAccountsL . ix idx . walletAccountSelectedL %= not

performNewAccount :: WidgetEventM WalletWidgetState p ()
performNewAccount = do
  UiLangFace{..} <- use walletLangFaceL
  name <- use walletNewAccountNameL
  use walletNewAccountResultL >>= \case
    NewAccountResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiNewAccount $ UiNewAccountArgs name) >>=
      assign walletNewAccountResultL . either NewAccountResultError NewAccountResultWaiting
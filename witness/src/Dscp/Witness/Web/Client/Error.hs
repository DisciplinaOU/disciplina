module Dscp.Witness.Web.Client.Error
    ( WitnessClientError (..)
    , servantToWitnessError
    ) where

import qualified Data.Text.Buildable as B
import Network.HTTP.Types (statusCode)
import Servant.Client (GenResponse (..), ServantError (..))
import qualified Text.Show

import Dscp.Witness.Web.Error

data WitnessClientError
    = WitnessClientError !WitnessWebError
    | SomeClientError !Text

instance Show WitnessClientError where
    show = toString . pretty

instance Buildable WitnessClientError where
    build = \case
        WitnessClientError err -> B.build err
        SomeClientError msg -> B.build msg

instance Exception WitnessClientError

servantToWitnessError :: ServantError -> WitnessClientError
servantToWitnessError servantError =
    maybe (SomeClientError $ show servantError) WitnessClientError mWalletError
  where
    mWalletError = do
        FailureResponse Response{..} <- pure servantError
        let body = decodeUtf8 responseBody
        case statusCode responseStatusCode of
            404 -> pure $ EntityAbsent body
            500 -> pure $ InternalError body
            _   -> Nothing

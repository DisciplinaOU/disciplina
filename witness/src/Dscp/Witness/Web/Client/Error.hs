module Dscp.Witness.Web.Client.Error
    ( WitnessClientError (..)
    , servantToWitnessError
    ) where

import Data.Aeson (decode)
import qualified Data.Text.Buildable as B
import Servant.Client (GenResponse (..), ServantError (..))
import qualified Text.Show

import Dscp.Witness.Web.Error

data WitnessClientError
    = WitnessClientError !WitnessAPIError
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
        errResponse <- decode @ErrResponse responseBody
        return (erContent errResponse)

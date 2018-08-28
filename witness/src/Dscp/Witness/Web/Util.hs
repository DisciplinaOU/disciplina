module Dscp.Witness.Web.Util
    ( snowdropErrorToShortJSON
    , parseShortJSONToSnowdropError
    , snowdropToServantErrNoReason
    ) where

import qualified Data.Map as M
import Servant (ServantErr, err400, err403, err500)

import Dscp.Snowdrop (AccountValidationException (..))

-- We rely on the fact that 'AccountValidationException' has only no-arg
-- constructors.
snowdropErrorToShortJSON :: AccountValidationException -> Text
snowdropErrorToShortJSON = show

parseShortJSONToSnowdropError :: Text -> Maybe AccountValidationException
parseShortJSONToSnowdropError msg =
    M.lookup msg accountErrors
  where
    accountErrors =
        M.fromList $ [minBound .. maxBound] <&> \err -> (show err, err)

snowdropToServantErrNoReason :: AccountValidationException -> ServantErr
snowdropToServantErrNoReason = \case
    AuthorDoesNotExist          -> err403
    SignatureIsMissing          -> err500
    SignatureIsCorrupted        -> err400
    TransactionIsCorrupted      -> err500
    NotASingletonSelfUpdate     -> err400
    NonceMustBeIncremented      -> err403
    PaymentMustBePositive       -> err400
    ReceiverOnlyGetsMoney       -> err500
    ReceiverMustIncreaseBalance -> err400
    SumMustBeNonNegative        -> err400

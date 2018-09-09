module Dscp.Snowdrop.Instances () where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Data (toConstr)
import Servant (err400, err403, err500)

import Dscp.Snowdrop.Types
import Dscp.Web.Class

----------------------------------------------------------------------------
-- JSON instances
----------------------------------------------------------------------------

deriveJSON defaultOptions ''AccountValidationException

----------------------------------------------------------------------------
-- Instances for exceptions
----------------------------------------------------------------------------

instance HasErrorTag AccountValidationException where
    errorTag = show . toConstr

instance ToServantErrNoReason AccountValidationException where
    toServantErrNoReason = \case
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
        CannotAffordFees            -> err403
        BalanceCannotBecomeNegative -> err403

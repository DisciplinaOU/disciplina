module Dscp.Snowdrop.Instances () where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Data (toConstr)
import Servant (err400, err403, err404, err500)

import Dscp.Snowdrop.Types
import Dscp.Web.Class
import Dscp.Witness.Logic.Exceptions

----------------------------------------------------------------------------
-- JSON instances
----------------------------------------------------------------------------

deriveJSON defaultOptions ''AccountException
deriveJSON defaultOptions ''LogicException

----------------------------------------------------------------------------
-- Instances for exceptions
----------------------------------------------------------------------------

instance HasErrorTag AccountException where
    errorTag = show . toConstr

instance HasErrorTag LogicException where
    errorTag = show . toConstr

instance ToServantErrNoReason AccountException where
    toServantErrNoReason = \case
        MTxNoOutputs                -> err400
        MTxDuplicateOutputs         -> err400
        InsufficientFees            -> err400
        SignatureIsMissing          -> err500
        SignatureIsCorrupted        -> err400
        TransactionIsCorrupted      -> err500
        NotASingletonSelfUpdate     -> err400
        NonceMustBeIncremented      -> err403
        PaymentMustBePositive       -> err500
        ReceiverOnlyGetsMoney       -> err500
        ReceiverMustIncreaseBalance -> err500
        SumMustBeNonNegative        -> err400
        CannotAffordFees            -> err403
        BalanceCannotBecomeNegative -> err403
        AccountInternalError _      -> err500

instance ToServantErrNoReason LogicException where
    toServantErrNoReason = \case
        LEBlockAbsent{} -> err404
        LETxAbsent{} -> err404
        LEMalformed{} -> err500

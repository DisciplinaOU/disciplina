module Dscp.Snowdrop.Instances () where

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Servant (err400, err403, err404, err500)

import Dscp.Core.Aeson ()
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

{- NOTE: everything in this module directly affects the witness API, keep 'witness.yaml'
document updated.
-}

instance HasErrorTag AccountException where
    errorTag = \case
        MTxNoOutputs{}                -> "NoOutputs"
        MTxDuplicateOutputs{}         -> "DuplicatedOutputs"
        TransactionAlreadyExists{}    -> "TransactionAlreadyExists"
        InsufficientFees{}            -> "InsufficientFees"
        SignatureIsMissing{}          -> "SignatureIsMissing"
        SignatureIsCorrupted{}        -> "SignatureIsCorrupted"
        TransactionIsCorrupted{}      -> "TransactionIsCorrupted"
        NonceMismatch{}               -> "NonceMismatch"
        PaymentMustBePositive{}       -> "PaymentMustBePositive"
        ReceiverOnlyGetsMoney{}       -> "ReceiverOnlyGetsMoney"
        OutputIsEmpty{}               -> "OutputIsEmpty"
        SumMustBeNonNegative{}        -> "SumMustBeNonNegative"
        CannotAffordOutputs{}         -> "CannotAffordOutputs"
        CannotAffordFees{}            -> "CannotAffordFees"
        WitnessMismatchesInput{}      -> "WitnessMismatchesInput"

instance ToServantErr AccountException where
    toServantErrNoBody = \case
        MTxNoOutputs{}                -> err400
        MTxDuplicateOutputs{}         -> err400
        TransactionAlreadyExists{}    -> err403
        InsufficientFees{}            -> err400
        SignatureIsMissing{}          -> err500
        SignatureIsCorrupted{}        -> err400
        TransactionIsCorrupted{}      -> err500
        NonceMismatch{}               -> err403
        PaymentMustBePositive{}       -> err500
        ReceiverOnlyGetsMoney{}       -> err500
        OutputIsEmpty{}               -> err400
        SumMustBeNonNegative{}        -> err400
        CannotAffordOutputs{}         -> err403
        CannotAffordFees{}            -> err403
        WitnessMismatchesInput{}      -> err400

instance HasErrorTag LogicException where
    errorTag = \case
        LEBlockAbsent{} -> "BlockNotFound"
        LETxAbsent{} -> "TransactionNotFound"
        LEMalformed{} -> "InternalError"
        LEPrivateBlockAbsent{} -> "PrivateBlockNotFound"

instance ToServantErr LogicException where
    toServantErrNoBody = \case
        LEBlockAbsent{} -> err404
        LETxAbsent{} -> err404
        LEMalformed{} -> err500
        LEPrivateBlockAbsent{} -> err404

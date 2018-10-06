{-# LANGUAGE StrictData #-}

module Dscp.Witness.Web.Error
    ( WitnessAPIError (..)
    , ErrResponse (..)
    , DSON
    , witnessToServantErr
    ) where

import Data.Aeson (encode)
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Reflection (Reifies (..))
import qualified Data.Text.Buildable as B
import Data.Typeable (cast)
import Servant (ServantErr (..), err400, err404, err500, err503)

import Dscp.Snowdrop
import Dscp.Util.Servant
import Dscp.Web.Class
import Dscp.Witness.Logic.Exceptions
import Dscp.Witness.Relay (RelayException)

-- | All kinds of SD exceptions which we care about.
data SdExceptions
    = SdAccountError AccountException
    | SdLogicError LogicException
    deriving (Show)

instance Buildable SdExceptions where
    build = \case
        SdAccountError err -> B.build err
        SdLogicError err -> B.build err

fromSnowdropException :: Exceptions -> Maybe SdExceptions
fromSnowdropException = \case
    AccountError e -> pure $ SdAccountError e
    LogicError e -> pure $ SdLogicError e
    PublicationError{} -> mzero
    BlockApplicationError{} -> mzero
    SdInternalError{} -> mzero

-- | All witness API exceptions.
data WitnessAPIError
    = BlockNotFound
    | TransactionNotFound
    | SdError SdExceptions
    | InternalError Text
    | ServiceUnavailable Text
    | InvalidFormat
    deriving (Show, Generic, Typeable)

instance Buildable WitnessAPIError where
    build = \case
        BlockNotFound -> "Specified block does not exist."
        TransactionNotFound -> "Specified transaction does not exist."
        SdError err -> B.build err
        InternalError msg -> "Internal error: " <> B.build msg
        ServiceUnavailable msg -> "Service unavailable: " <> B.build msg
        InvalidFormat -> "Failed to deserialise one of parameters."

instance Exception WitnessAPIError where
    fromException e@(SomeException e') =
        asum
        [ cast e'
        , fmap SdError . fromSnowdropException =<< fromException e
        , ServiceUnavailable . show @Text @RelayException <$> fromException e
        ]

-- | Contains info about error in client-convenient form.
data ErrResponse = ErrResponse
    { erError   :: !Text
      -- ^ Enum which identifies type of error, for frontend
    , erContent :: !WitnessAPIError
      -- ^ Error itself, to allow client deserialise the error
    } deriving (Show, Generic)

toErrResponse :: WitnessAPIError -> ErrResponse
toErrResponse err =
    ErrResponse
    { erContent = err
    , erError = errorTag err
    }

-- | Make up error which will be returned to client.
witnessToServantErr :: WitnessAPIError -> ServantErr
witnessToServantErr err =
    (toServantErrNoReason err){ errBody = encode $ toErrResponse err }

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''SdExceptions
deriveJSON defaultOptions ''WitnessAPIError
deriveJSON defaultOptions ''ErrResponse

---------------------------------------------------------------------------
-- Error instances
---------------------------------------------------------------------------

instance HasErrorTag SdExceptions where
    errorTag = \case
        SdAccountError e -> errorTag e
        SdLogicError e -> errorTag e

instance HasErrorTag WitnessAPIError where
    errorTag = \case
        BlockNotFound -> "BlockNotFound"
        TransactionNotFound -> "TransactionNotFound"
        SdError err -> errorTag err
        InternalError{} -> "InternalError"
        ServiceUnavailable{} -> "ServiceUnavailable"
        InvalidFormat -> "InvalidFormat"

instance ToServantErrNoReason SdExceptions where
    toServantErrNoReason = \case
        SdAccountError e -> toServantErrNoReason e
        SdLogicError e -> toServantErrNoReason e

instance ToServantErrNoReason WitnessAPIError where
    toServantErrNoReason = \case
        BlockNotFound        -> err404
        TransactionNotFound  -> err404
        SdError err          -> toServantErrNoReason err
        InternalError{}      -> err500
        ServiceUnavailable{} -> err503
        InvalidFormat        -> err400

---------------------------------------------------------------------------
-- Other
---------------------------------------------------------------------------

data WitnessDecodeErrTag
instance Reifies WitnessDecodeErrTag String where
    reflect _ = decodeUtf8 $ encode InvalidFormat

-- | Marker like 'JSON' for servant, but returns just "InvalidFormat" on
-- decoding error.
type DSON = SimpleJSON WitnessDecodeErrTag
